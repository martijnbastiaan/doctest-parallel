module NonExposedModule.NoImplicitImport where

{-# ANN module "doctest-parallel: --no-implicit-module-import" #-}

-- |
-- >>> import NonExposedModule.Exposed (foo)
-- >>> foo 7
-- 14
foo :: Int -> Int
foo a = a + a
