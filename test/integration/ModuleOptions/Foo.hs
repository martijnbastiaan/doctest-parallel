module ModuleOptions.Foo where

{-# ANN module "doctest-parallel: --preserve-it" #-}

-- |
--
-- >>> :t 'a'
-- 'a' :: Char
--
-- >>> "foo"
-- "foo"
--
-- >>> length it
-- 3
--
-- >>> it * it
-- 9
--
-- >>> :t it
-- it :: Int
--
foo = undefined
