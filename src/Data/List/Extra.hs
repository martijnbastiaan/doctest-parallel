module Data.List.Extra (trim, splitOn, dropEnd) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Remove spaces from either side of a string. A combination of 'trimEnd' and 'trimStart'.
--
-- > trim      "  hello   " == "hello"
-- > trimStart "  hello   " == "hello   "
-- > trimEnd   "  hello   " == "  hello"
-- > \s -> trim s == trimEnd (trimStart s)
trim :: String -> String
trim = trimEnd . trimStart

-- | Remove spaces from the start of a string, see 'trim'.
trimStart :: String -> String
trimStart = dropWhile isSpace

-- | Remove spaces from the end of a string, see 'trim'.
trimEnd :: String -> String
trimEnd = dropWhileEnd isSpace

-- TODO: Use doctests after fixing: https://github.com/martijnbastiaan/doctest-parallel/issues/87

-- | Break a list into pieces separated by the first argument, consuming the delimiter.
--
-- > splitOn '.' "A.B"
-- ["A","B"]
-- > splitOn '.' "A.B.C"
-- ["A","B","C"]
-- > splitOn '.' "."
-- ["",""]
-- > splitOn '.' ""
-- [""]
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn needle haystack =
  case break (== needle) haystack of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitOn needle rest

-- | Drop a number of elements from the end of the list. Unlike 'init',
-- @dropEnd 1@ doesn't fail on empty lists.
--
-- > dropEnd 3 "hello"
-- "he"
-- > dropEnd 5 "bye"
-- ""
-- > dropEnd (-1) "bye"
-- "bye"
dropEnd :: Int -> [a] -> [a]
dropEnd i xs
  | i <= 0 = xs
  | otherwise = go xs (drop i xs)
 where
  go (y:ys) (_:zs) = y : go ys zs
  go _ _ = []
