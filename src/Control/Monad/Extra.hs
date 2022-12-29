module Control.Monad.Extra where

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate t f = do b <- predicate; if b then t else f
