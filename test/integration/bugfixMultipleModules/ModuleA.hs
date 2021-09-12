-- | The test below should fail, as ModuleB does not export it:
--
-- >>> fib 10
-- 55
module ModuleA where

import ModuleB
