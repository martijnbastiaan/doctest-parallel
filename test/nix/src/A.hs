{-# LANGUAGE CPP #-}

module A where

-- Test CPP on 'ghc', which seems to be handled specially
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

-- Test CPP on 'base', a wired-in package
#if MIN_VERSION_base(4,12,0)
x = 3
#else
x = 5
#endif

-- Test CPP on 'extra', a "normal" Hackage package
#if MIN_VERSION_extra(1,7,0)
y = 10
#else
y = 20
#endif
