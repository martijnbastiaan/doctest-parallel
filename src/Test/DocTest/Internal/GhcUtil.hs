{-# LANGUAGE CPP #-}
module Test.DocTest.Internal.GhcUtil (withGhc) where

import           GHC.Paths (libdir)
import           GHC
#if __GLASGOW_HASKELL__ < 900
import           DynFlags (gopt_set)
#else
import           GHC.Driver.Session (gopt_set)
#endif

#if __GLASGOW_HASKELL__ < 900
import           Panic (throwGhcException)
#else
import           GHC.Utils.Panic (throwGhcException)
#endif

-- | Run a GHC action in Haddock mode
withGhc :: [String] -> Ghc a -> IO a
withGhc flags action = do
  flags_ <- handleStaticFlags flags

  runGhc (Just libdir) $ do
    handleDynamicFlags flags_
    action

handleStaticFlags :: [String] -> IO [Located String]
handleStaticFlags flags = return $ map noLoc $ flags

handleDynamicFlags :: GhcMonad m => [Located String] -> m ()
handleDynamicFlags flags = do
#if __GLASGOW_HASKELL__ >= 901
  logger <- getLogger
  let parseDynamicFlags' = parseDynamicFlags logger
#else
  let parseDynamicFlags' = parseDynamicFlags
#endif
  dynflags0 <- setHaddockMode <$> getSessionDynFlags
  (dynflags1, locSrcs, _) <- parseDynamicFlags' dynflags0 flags
  _ <- setSessionDynFlags dynflags1

  -- We basically do the same thing as `ghc/Main.hs` to distinguish
  -- "unrecognised flags" from source files.
  let srcs = map unLoc locSrcs
      unknown_opts = [ f | f@('-':_) <- srcs ]
  case unknown_opts of
    opt : _ -> throwGhcException (UsageError ("unrecognized option `"++ opt ++ "'"))
    _       -> return ()

setHaddockMode :: DynFlags -> DynFlags
setHaddockMode dynflags = (gopt_set dynflags Opt_Haddock) {
#if __GLASGOW_HASKELL__ >= 906
      backend   = noBackend
#elif __GLASGOW_HASKELL__ >= 901
      backend   = NoBackend
#else
      hscTarget = HscNothing
#endif
    , ghcMode   = CompManager
    , ghcLink   = NoLink
    }
