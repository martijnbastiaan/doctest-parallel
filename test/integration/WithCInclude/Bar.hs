{-# LANGUAGE CPP #-}

module WithCInclude.Bar where

#include "WithCInclude.h"


-- |
-- >>> x
-- 42
x :: Int
x = THE_DEFINE
