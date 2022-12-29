module Control.Monad.Extra where

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate t f = do b <- predicate; if b then t else f

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())
