module ModuleIsolation.TestA (foo) where

import ModuleIsolation.TestB ()

{- $setup
>>> :set -XLambdaCase
-}

-- |
-- >>> (\case { 3 -> 5; 7 -> 9}) 3
-- 5
foo :: Num a => a
foo = 3
