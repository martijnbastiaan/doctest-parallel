{-# LANGUAGE CPP #-}

module MyLib (add, main) where

-- | Adds two 'Int's
--
-- >>> ADD 3 5
-- 8
add :: Int -> Int -> Int
add = (+)

main :: IO ()
main = print (ADD 3 5)
