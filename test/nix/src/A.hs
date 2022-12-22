{-# LANGUAGE CPP #-}

module A where

#if MIN_VERSION_ghc(9,0,0)
-- #if MIN_VERSION_base(4,15,0)
-- |
-- >>> foo
-- 23
foo = 23
#else
-- |
-- >>> bar
-- 42
bar = 42
#endif
