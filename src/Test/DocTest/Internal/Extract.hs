{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest.Internal.Extract (Module(..), extract, eraseConfigLocation) where

import           Prelude hiding (mod, concat)
import           Control.Monad
import           Control.Exception
import           Data.List (partition, isPrefixOf)
import           Data.List.Extra (trim)
import           Data.Maybe

import           Control.DeepSeq (NFData, deepseq)
import           Data.Generics (Data, Typeable, extQ, mkQ, everythingBut)

import qualified GHC

#if __GLASGOW_HASKELL__ < 900
import           GHC hiding (Module, Located, moduleName)
import           DynFlags
import           MonadUtils (liftIO)
#else
import           GHC hiding (Module, Located, moduleName)
import           GHC.Driver.Session
import           GHC.Utils.Monad (liftIO)
#endif

#if __GLASGOW_HASKELL__ < 900
import           Digraph (flattenSCCs)
import           Exception (ExceptionMonad)
#else
import           GHC.Data.Graph.Directed (flattenSCCs)
import           GHC.Utils.Exception (ExceptionMonad)
import           Control.Monad.Catch (generalBracket)
#endif

import           System.Directory
import           System.FilePath

#if __GLASGOW_HASKELL__ < 900
import           BasicTypes (SourceText(SourceText))
import           FastString (unpackFS)
#elif __GLASGOW_HASKELL__ < 902
import           GHC.Data.FastString (unpackFS)
import           GHC.Types.Basic (SourceText(SourceText))
#else
import           GHC.Data.FastString (unpackFS)
import           GHC.Types.SourceText (SourceText(SourceText))
#endif

import           System.Posix.Internals (c_getpid)

import           Test.DocTest.Internal.GhcUtil (withGhc)
import           Test.DocTest.Internal.Location hiding (unLoc)
import           Test.DocTest.Internal.Util (convertDosLineEndings)

#if __GLASGOW_HASKELL__ >= 806
#if __GLASGOW_HASKELL__ < 900
import           DynamicLoading (initializePlugins)
#else
import           GHC.Runtime.Loader (initializePlugins)
#endif
#endif

#if __GLASGOW_HASKELL__ >= 901
import           GHC.Unit.Module.Graph
#endif

import           GHC.Generics (Generic)


-- | A wrapper around `SomeException`, to allow for a custom `Show` instance.
newtype ExtractError = ExtractError SomeException
  deriving Typeable

instance Show ExtractError where
  show (ExtractError e) =
    unlines [
        "Ouch! Hit an error thunk in GHC's AST while extracting documentation."
      , ""
      , "    " ++ msg
      , ""
      , "This is most likely a bug in doctest-parallel."
      , ""
      , "Please report it here: https://github.com/martijnbastiaan/doctest-parallel/issues/new"
      ]
    where
      msg = case fromException e of
        Just (Panic s) -> "GHC panic: " ++ s
        _              -> show e

instance Exception ExtractError

-- | Documentation for a module grouped together with the modules name.
data Module a = Module {
  moduleName    :: String
, moduleSetup   :: Maybe a
, moduleContent :: [a]
, moduleConfig  :: [Located String]
} deriving (Eq, Functor, Show, Generic, NFData)

eraseConfigLocation :: Module a -> Module a
eraseConfigLocation m@Module{moduleConfig} =
  m{moduleConfig=map go moduleConfig}
 where
  go (Located _ a) = noLocation a

#if __GLASGOW_HASKELL__ < 803
type GhcPs = RdrName
#endif

#if __GLASGOW_HASKELL__ < 805
addQuoteInclude :: [String] -> [String] -> [String]
addQuoteInclude includes new = new ++ includes
#endif

-- | Parse a list of modules.
parse :: [String] -> IO [ParsedModule]
parse args = withGhc args $ \modules -> withTempOutputDir $ do
  setTargets =<< forM modules (\ m -> guessTarget m
#if __GLASGOW_HASKELL__ >= 903
                Nothing
#endif
                Nothing)
  mods <- depanal [] False

  let sortedMods = flattenSCCs
#if __GLASGOW_HASKELL__ >= 901
                     $ filterToposortToModules
#endif
                     $ topSortModuleGraph False mods Nothing
  reverse <$> mapM (loadModPlugins >=> parseModule) sortedMods

  where
    -- copied from Haddock/GhcUtils.hs
    modifySessionDynFlags :: (DynFlags -> DynFlags) -> Ghc ()
    modifySessionDynFlags f = do
      dflags <- getSessionDynFlags
      let dflags' = case lookup "GHC Dynamic" (compilerInfo dflags) of
            Just "YES" -> gopt_set dflags Opt_BuildDynamicToo
            _          -> dflags
      _ <- setSessionDynFlags (f dflags')
      return ()

    withTempOutputDir :: Ghc a -> Ghc a
    withTempOutputDir action = do
      tmp <- liftIO getTemporaryDirectory
      x   <- liftIO c_getpid
      let dir = tmp </> ".doctest-" ++ show x
      modifySessionDynFlags (setOutputDir dir)
      gbracket_
        (liftIO $ createDirectory dir)
        (liftIO $ removeDirectoryRecursive dir)
        action

    -- | A variant of 'gbracket' where the return value from the first computation
    -- is not required.
    gbracket_ :: ExceptionMonad m => m a -> m b -> m c -> m c
#if __GLASGOW_HASKELL__ < 900
    gbracket_ before_ after thing = gbracket before_ (const after) (const thing)
#else
    gbracket_ before_ after thing = fst <$> generalBracket before_ (\ _ _ -> after) (const thing)
#endif

    setOutputDir f d = d {
        objectDir  = Just f
      , hiDir      = Just f
      , stubDir    = Just f
      , includePaths = addQuoteInclude (includePaths d) [f]
      }


#if __GLASGOW_HASKELL__ >= 806
    -- Since GHC 8.6, plugins are initialized on a per module basis
    loadModPlugins modsum = do
      _ <- setSessionDynFlags (GHC.ms_hspp_opts modsum)
      hsc_env <- getSession

# if __GLASGOW_HASKELL__ >= 901
      hsc_env' <- liftIO (initializePlugins hsc_env)
      setSession hsc_env'
      return $ modsum
# else
      dynflags' <- liftIO (initializePlugins hsc_env (GHC.ms_hspp_opts modsum))
      return $ modsum { ms_hspp_opts = dynflags' }
# endif
#else
    loadModPlugins = return
#endif

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
extract :: [String] -> IO [Module (Located String)]
extract args = do
  mods <- parse args
  let docs = map (fmap (fmap convertDosLineEndings) . extractFromModule) mods

  (docs `deepseq` return docs) `catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      Handler (\e -> throw (e :: AsyncException))
    , Handler (throwIO . ExtractError)
    ]

-- | Extract all docstrings from given module and attach the modules name.
extractFromModule :: ParsedModule -> Module (Located String)
extractFromModule m = Module
  { moduleName = name
  , moduleSetup = listToMaybe (map snd setup)
  , moduleContent = map snd docs
  , moduleConfig = moduleAnnsFromModule m
  }
 where
  isSetup = (== Just "setup") . fst
  (setup, docs) = partition isSetup (docStringsFromModule m)
  name = (moduleNameString . GHC.moduleName . ms_mod . pm_mod_summary) m

-- | Extract all module annotations from given module.
moduleAnnsFromModule :: ParsedModule -> [Located String]
moduleAnnsFromModule mod =
  [fmap stripOptionString ann | ann <- anns, isOption ann]
 where
  optionPrefix = "doctest-parallel:"
  isOption (Located _ s) = optionPrefix `isPrefixOf` s
  stripOptionString s = trim (drop (length optionPrefix) s)
  anns = extractModuleAnns source
  source = (unLoc . pm_parsed_source) mod

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedModule -> [(Maybe String, Located String)]
docStringsFromModule mod =
#if __GLASGOW_HASKELL__ < 904
  map (fmap (toLocated . fmap unpackHDS)) docs
#else
  map (fmap (toLocated . fmap renderHsDocString)) docs
#endif
 where
  source = (unLoc . pm_parsed_source) mod

  -- we use dlist-style concatenation here
  docs :: [(Maybe String, LHsDocString)]
  docs = header ++ exports ++ decls

  -- We process header, exports and declarations separately instead of
  -- traversing the whole source in a generic way, to ensure that we get
  -- everything in source order.
  header :: [(Maybe String, LHsDocString)]
#if __GLASGOW_HASKELL__ < 904
  header  = [(Nothing, x) | Just x <- [hsmodHaddockModHeader source]]
#else
  header = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader source]]
#endif

  exports :: [(Maybe String, LHsDocString)]
  exports = [ (Nothing, L (locA loc) doc)
#if __GLASGOW_HASKELL__ < 710
            | L loc (IEDoc doc) <- concat (hsmodExports source)
#elif __GLASGOW_HASKELL__ < 805
            | L loc (IEDoc doc) <- maybe [] unLoc (hsmodExports source)
#elif __GLASGOW_HASKELL__ < 904
            | L loc (IEDoc _ doc) <- maybe [] unLoc (hsmodExports source)
#else
            | L loc (IEDoc _ (unLoc . fmap hsDocString -> doc)) <- maybe [] unLoc (hsmodExports source)
#endif
            ]

  decls :: [(Maybe String, LHsDocString)]
  decls   = extractDocStrings (Right (hsmodDecls source))

type Selector b a = a -> ([b], Bool)

type DocSelector a = Selector (Maybe String, LHsDocString) a
type AnnSelector a = Selector (Located String) a

-- | Collect given value and descend into subtree.
select :: a -> ([a], Bool)
select x = ([x], False)

#if __GLASGOW_HASKELL__ >= 904
-- | Don't collect any values
noSelect :: ([a], Bool)
noSelect = ([], False)
#endif

-- | Extract module annotations from given value.
extractModuleAnns :: Data a => a -> [Located String]
extractModuleAnns = everythingBut (++) (([], False) `mkQ` fromLHsDecl)
 where
  fromLHsDecl :: AnnSelector (LHsDecl GhcPs)
  fromLHsDecl (L (locA -> loc) decl) = case decl of
#if __GLASGOW_HASKELL__ < 805
    AnnD (HsAnnotation (SourceText _) ModuleAnnProvenance (L _loc expr))
#else
    AnnD _ (HsAnnotation _ (SourceText _) ModuleAnnProvenance (L _loc expr))
#endif
     | Just s <- extractLit loc expr
     -> select s
    _ ->
      -- XXX: Shouldn't this be handled by 'everythingBut'?
      (extractModuleAnns decl, True)

-- | Extract string literals. Looks through type annotations and parentheses.
extractLit :: SrcSpan -> HsExpr GhcPs -> Maybe (Located String)
extractLit loc = \case
  -- well this is a holy mess innit
#if __GLASGOW_HASKELL__ < 805
  HsPar (L l e) -> extractLit l e
  ExprWithTySig (L l e) _ -> extractLit l e
  HsOverLit OverLit{ol_val=HsIsString _ s} -> Just (toLocated (L loc (unpackFS s)))
  HsLit (HsString _ s) -> Just (toLocated (L loc (unpackFS s)))
  _ -> Nothing
#else
#if __GLASGOW_HASKELL__ < 904
  HsPar _ (L l e) -> extractLit (locA l) e
#else
  HsPar _ _ (L l e) _ -> extractLit (locA l) e
#endif
#if __GLASGOW_HASKELL__ < 807
  ExprWithTySig _ (L l e) -> extractLit l e
#else
  ExprWithTySig _ (L l e) _ -> extractLit (locA l) e
#endif
  HsOverLit _ OverLit{ol_val=HsIsString _ s} -> Just (toLocated (L loc (unpackFS s)))
  HsLit _ (HsString _ s) -> Just (toLocated (L loc (unpackFS s)))
  _ -> Nothing
#endif

-- | Extract all docstrings from given value.
extractDocStrings :: Either (HsDecl GhcPs) [LHsDecl GhcPs] -> [(Maybe String, LHsDocString)]
extractDocStrings =
  everythingBut
    (++)
    (        ([], False)
      `mkQ`  fromLHsDecl
      `extQ` fromLDocDecl
      `extQ` fromLHsDocString
#if __GLASGOW_HASKELL__ >= 904
      `extQ` fromHsType
#endif
    )
  where
    fromLHsDecl :: DocSelector (LHsDecl GhcPs)
    fromLHsDecl (L loc decl) = case decl of

      -- Top-level documentation has to be treated separately, because it has
      -- no location information attached.  The location information is
      -- attached to HsDecl instead.
#if __GLASGOW_HASKELL__ < 805
      DocD x
#else
      DocD _ x
#endif
           -> select (fromDocDecl (locA loc) x)

      _ -> (extractDocStrings (Left decl), True)


    fromLDocDecl :: DocSelector
#if __GLASGOW_HASKELL__ >= 901
                             (LDocDecl GhcPs)
#else
                             LDocDecl
#endif
    fromLDocDecl (L loc x) = select (fromDocDecl (locA loc) x)

    fromLHsDocString :: DocSelector LHsDocString
    fromLHsDocString x = select (Nothing, x)

#if __GLASGOW_HASKELL__ >= 904
    fromHsType :: DocSelector (HsType GhcPs)
    fromHsType x = case x of
      HsDocTy _ _ (L loc hsDoc) -> select (Nothing, L loc (hsDocString hsDoc))
      _ -> noSelect
#endif

#if __GLASGOW_HASKELL__ < 904
    fromDocDecl :: SrcSpan -> DocDecl -> (Maybe String, LHsDocString)
#else
    fromDocDecl :: SrcSpan -> DocDecl GhcPs -> (Maybe String, LHsDocString)
#endif
    fromDocDecl loc x = case x of
#if __GLASGOW_HASKELL__ < 904
      DocCommentNamed name doc -> (Just name, L loc doc)
      _                        -> (Nothing, L loc $ docDeclDoc x)
#else
      DocCommentNamed name doc -> (Just name, hsDocString <$> doc)
      _                        -> (Nothing, L loc $ hsDocString $ unLoc $ docDeclDoc x)
#endif

#if __GLASGOW_HASKELL__ < 805
-- | Convert a docstring to a plain string.
unpackHDS :: HsDocString -> String
unpackHDS (HsDocString s) = unpackFS s
#endif

#if __GLASGOW_HASKELL__ < 901
locA :: SrcSpan -> SrcSpan
locA = id
#endif
