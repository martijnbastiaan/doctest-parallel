{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest.Internal.Extract (Module(..), isEmptyModule, extract, eraseConfigLocation) where
import           Prelude hiding (mod, concat)
import           Control.DeepSeq (NFData, deepseq)
import           Control.Exception
import           Control.Monad
import           Data.Generics (Data, extQ, mkQ, everythingBut)
import           Data.List (partition, isPrefixOf)
import           Data.List.Extra (trim, splitOn)
import           Data.Maybe
import           GHC.Generics (Generic)

#if __GLASGOW_HASKELL__ < 912
import           Data.Generics (Typeable)
#endif

#if __GLASGOW_HASKELL__ < 900
import           GHC hiding (Module, Located, moduleName, parsedSource)
import           DynFlags
import           MonadUtils (liftIO)
#else
import           GHC hiding (Module, Located, moduleName, parsedSource)
import           GHC.Driver.Session
import           GHC.Utils.Monad (liftIO)
#endif

import           System.Directory
import           System.FilePath

#if __GLASGOW_HASKELL__ < 900
import           BasicTypes (SourceText(SourceText))
import           FastString (unpackFS)
#elif __GLASGOW_HASKELL__ < 902
import           GHC.Data.FastString (unpackFS)
import           GHC.Types.Basic (SourceText(SourceText))
#elif __GLASGOW_HASKELL__ < 906
import           GHC.Types.SourceText (SourceText(SourceText))
import           GHC.Data.FastString (unpackFS)
#else
import           GHC.Data.FastString (unpackFS)
#endif

import           Test.DocTest.Internal.GhcUtil (withGhc)
import           Test.DocTest.Internal.Location hiding (unLoc)
import           Test.DocTest.Internal.Util (convertDosLineEndings)

#if MIN_VERSION_ghc_exactprint(1,3,0)
import           Language.Haskell.GHC.ExactPrint.Parsers (parseModuleEpAnnsWithCppInternal, defaultCppOptions)
#else
import           Language.Haskell.GHC.ExactPrint.Parsers (parseModuleApiAnnsWithCppInternal, defaultCppOptions)
#endif

#if __GLASGOW_HASKELL__ < 900
import           HscTypes (throwErrors)
import           HeaderInfo (getOptionsFromFile)
#elif __GLASGOW_HASKELL__ < 902
import           GHC.Driver.Types (throwErrors)
import           GHC.Parser.Header (getOptionsFromFile)
#elif __GLASGOW_HASKELL__ < 904
import           GHC.Types.SourceError (throwErrors)
import           GHC.Parser.Header (getOptionsFromFile)
#else
import           GHC.Types.SourceError (throwErrors)
import           GHC.Parser.Header (getOptionsFromFile)
import           GHC.Driver.Config.Parser (initParserOpts)
#endif

#if __GLASGOW_HASKELL__ < 904
initParserOpts :: DynFlags -> DynFlags
initParserOpts = id
#endif


-- | A wrapper around `SomeException`, to allow for a custom `Show` instance.
newtype ExtractError = ExtractError SomeException
#if __GLASGOW_HASKELL__ < 912
  deriving Typeable
#endif

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

data ModuleNotFoundError = ModuleNotFoundError String [FilePath]
  deriving (
#if __GLASGOW_HASKELL__ < 912
    Typeable,
#endif
    Exception
  )

instance Show ModuleNotFoundError where
  show (ModuleNotFoundError modName incdirs) =
    unlines [
        "Module not found: " ++ modName
      , ""
      , "Tried the following include directories:"
      , ""
      , unlines incdirs
      ]

-- | Documentation for a module grouped together with the modules name.
data Module a = Module {
  moduleName    :: String
, moduleSetup   :: Maybe a
, moduleContent :: [a]
, moduleConfig  :: [Located String]
} deriving (Eq, Functor, Show, Generic, NFData)

isEmptyModule :: Module a -> Bool
isEmptyModule (Module _ setup tests _) = null tests && isNothing setup

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

moduleParts :: String -> [String]
moduleParts = splitOn '.'

findModulePath :: [FilePath] -> String -> IO FilePath
findModulePath importPaths modName = do
  let
    modPath = foldl1 (</>) (moduleParts modName) <.> "hs"

  found <- fmap catMaybes $ forM importPaths $ \importPath -> do
    let fullPath = importPath </> modPath
    exists <- doesFileExist fullPath
    return $ if exists then Just fullPath else Nothing

  case found of
    [] -> throwIO (ModuleNotFoundError modName importPaths)
    (p:_) -> pure p

-- | Parse a list of modules. Can throw an `ModuleNotFoundError` if a module's
-- source file cannot be found. Can throw a `SourceError` if an error occurs
-- while parsing.
parse :: [String] -> String -> IO ParsedSource
parse args modName = do
  -- Find all specified modules on disk
  withGhc args $ do
    importPaths0 <- importPaths <$> getDynFlags
    path <- liftIO $ findModulePath importPaths0 modName

    -- LANGUAGE pragmas can influence how a file is parsed. For example, CPP
    -- means we need to preprocess the file before parsing it. We use GHC's
    -- `getOptionsFromFile` to parse these pragmas and then feed them as options
    -- to the "real" parser.
    dynFlags0 <- getDynFlags
#if __GLASGOW_HASKELL__ < 904
    flagsFromFile <-
#else
    (_, flagsFromFile) <-
#endif
      liftIO $ getOptionsFromFile (initParserOpts dynFlags0) path
    (dynFlags1, _, _) <- parseDynamicFilePragma dynFlags0 flagsFromFile

#if MIN_VERSION_ghc_exactprint(1,3,0)
    result <- parseModuleEpAnnsWithCppInternal defaultCppOptions dynFlags1 path
#else
    result <- parseModuleApiAnnsWithCppInternal defaultCppOptions dynFlags1 path
#endif

    case result of
      Left errs -> throwErrors errs
#if MIN_VERSION_ghc_exactprint(1,3,0)
      Right (_cppComments, _dynFlags, parsedSource) -> pure parsedSource
#else
      Right (_apiAnns, _cppComments, _dynFlags, parsedSource) -> pure parsedSource
#endif

-- | Extract all docstrings from given list of files/modules.
--
-- This includes the docstrings of all local modules that are imported from
-- those modules (possibly indirect).
--
-- Can throw `ExtractError` if an error occurs while extracting the docstrings,
-- or a `SourceError` if an error occurs while parsing the module. Can throw a
-- `ModuleNotFoundError` if a module's source file cannot be found.
extract :: [String] -> String -> IO (Module (Located String))
extract args modName = do
  mod <- parse args modName
  let
    docs0 = extractFromModule modName mod
    docs1 = fmap convertDosLineEndings <$> docs0

  (docs1 `deepseq` return docs1) `catches` [
      -- Re-throw AsyncException, otherwise execution will not terminate on
      -- SIGINT (ctrl-c).  All AsyncExceptions are re-thrown (not just
      -- UserInterrupt) because all of them indicate severe conditions and
      -- should not occur during normal operation.
      Handler (\e -> throw (e :: AsyncException))
    , Handler (throwIO . ExtractError)
    ]

-- | Extract all docstrings from given module and attach the modules name.
extractFromModule :: String -> ParsedSource -> Module (Located String)
extractFromModule modName m = Module
  { moduleName = modName
  , moduleSetup = listToMaybe (map snd setup)
  , moduleContent = map snd docs
  , moduleConfig = moduleAnnsFromModule m
  }
 where
  isSetup = (== Just "setup") . fst
  (setup, docs) = partition isSetup (docStringsFromModule m)

-- | Extract all module annotations from given module.
moduleAnnsFromModule :: ParsedSource -> [Located String]
moduleAnnsFromModule mod =
  [fmap stripOptionString ann | ann <- anns, isOption ann]
 where
  optionPrefix = "doctest-parallel:"
  isOption (Located _ s) = optionPrefix `isPrefixOf` s
  stripOptionString s = trim (drop (length optionPrefix) s)
  anns = extractModuleAnns source
  source = unLoc mod

-- | Extract all docstrings from given module.
docStringsFromModule :: ParsedSource -> [(Maybe String, Located String)]
docStringsFromModule mod =
#if __GLASGOW_HASKELL__ < 904
  map (fmap (toLocated . fmap unpackHDS)) docs
#else
  map (fmap (toLocated . fmap renderHsDocString)) docs
#endif
 where
  source = unLoc mod

  -- we use dlist-style concatenation here
  docs :: [(Maybe String, LHsDocString)]
  docs = header ++ exports ++ decls

  -- We process header, exports and declarations separately instead of
  -- traversing the whole source in a generic way, to ensure that we get
  -- everything in source order.
  header :: [(Maybe String, LHsDocString)]
#if __GLASGOW_HASKELL__ < 904
  header  = [(Nothing, x) | Just x <- [hsmodHaddockModHeader source]]
#elif __GLASGOW_HASKELL__ < 906
  header = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader source]]
#else
  header = [(Nothing, hsDocString <$> x) | Just x <- [hsmodHaddockModHeader (hsmodExt source)]]
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
#elif __GLASGOW_HASKELL__ < 906
    AnnD _ (HsAnnotation _ (SourceText _) ModuleAnnProvenance (L _loc expr))
#else
    AnnD _ (HsAnnotation _ ModuleAnnProvenance (L _loc expr))
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
#elif __GLASGOW_HASKELL__ < 909
  HsPar _ _ (L l e) _ -> extractLit (locA l) e
#else
  HsPar _ (L l e) -> extractLit (locA l) e
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
