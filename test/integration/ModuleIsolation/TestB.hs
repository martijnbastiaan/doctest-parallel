module ModuleIsolation.TestB (bar) where

-- | Example usage:
--
-- >>> (\case { 3 -> 5; 7 -> 9}) 3
-- 5
bar :: Num a => a
bar = 3
