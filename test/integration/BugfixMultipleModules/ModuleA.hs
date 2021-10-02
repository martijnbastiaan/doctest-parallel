{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | The test below should fail, as ModuleB does not export it:
--
-- >>> import BugfixMultipleModules.ModuleB
-- >>> fib 10
-- 55
module BugfixMultipleModules.ModuleA where
