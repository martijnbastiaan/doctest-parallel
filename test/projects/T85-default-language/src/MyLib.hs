module MyLib (someFunc) where

data (Show a) => ShowProxy a = Prox a
  deriving (Show)

showViaProxy :: (Show a) => a -> IO ()
showViaProxy a = putStrLn $ show $ Prox a

someFunc :: IO ()
someFunc = showViaProxy "hello world!"
