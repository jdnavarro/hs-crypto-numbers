{-# LANGUAGE MagicHash #-}
module Crypto.Number.F2M
    ( addF2M
    , mulF2M
    , modF2M
    , invF2M
    , divF2M
    ) where

import Control.Applicative ((<$>))
import GHC.Exts
import GHC.Integer.Logarithms (integerLog2#)
import Data.Bits (xor,shift,testBit)

addF2M :: Integer -> Integer -> Integer
addF2M = xor
{-# INLINE addF2M #-}

-- Long division
modF2M :: Integer -> Integer -> Integer
modF2M fx = go
  where
    go n | s == 0  = n `xor` fx
         | s < 0 = n
         | otherwise = go $ n `xor` shift fx s
      where
        s = log2 n - log2 fx
{-# INLINE modF2M #-}

-- Shift and add
mulF2M :: Integer -> Integer -> Integer -> Integer
mulF2M fx n1 n2 = modF2M fx
                $ go (if n2 `mod` 2 == 1 then n1 else 0) (log2 n2)
  where
    go n s | s == 0  = n
           | otherwise = if testBit n2 s
                            then go (n `xor` shift n1 s) (s - 1)
                            else go n (s - 1)

-- Extended Euclidean
invF2M :: Integer -> Integer -> Maybe Integer
invF2M fx n = go n fx 1 0
    where
      go u v g1 g2
          | u == 0 = Nothing
          | u == 1 = Just $ modF2M fx g1
          | otherwise = if j < 0
                           then go u (v `xor` shift u (-j))
                                   g1 (g2 `xor` shift g1 (-j))
                           else go (u `xor` shift v j) v
                                   (g1 `xor` shift g2 j) g2
        where
          j = log2 u - log2 v

divF2M :: Integer -> Integer -> Integer -> Maybe Integer
divF2M fx n1 n2 = mulF2M fx n1 <$> invF2M fx n2

log2 :: Integer -> Int
log2 0 = 0
log2 x = I# (integerLog2# x)
{-# INLINE log2 #-}
