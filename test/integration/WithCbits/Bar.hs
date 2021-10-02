{-# LANGUAGE ForeignFunctionInterface #-}
module WithCbits.Bar where

import Foreign.C

-- |
-- >>> foo
-- 23
foreign import ccall foo :: CInt
