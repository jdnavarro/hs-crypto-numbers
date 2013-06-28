module Crypto.Number.F2M
    ( addF2M
    , mulF2M
    , modF2M
    , invF2M
    ) where

import Data.Bits (xor,shift,testBit)

addF2M :: Integer -> Integer -> Integer
addF2M = xor

mulF2M :: Integer -> Integer -> Integer -> Integer
mulF2M fx n1 n2 =
    modF2M fx $ foldr (\x -> xor $ if testBit n2 x then shift n1 x else 0)
                      (if n2 `mod` 2 == 1 then n1 else 0)
                      [1 .. fromIntegral (log2 n2)]

modF2M :: Integer -> Integer -> Integer
modF2M fx = go
  where
    go r | s == 0  = r `xor` fx
         | otherwise = go $ r `xor` shift fx s
      where
        -- TODO: This is too slow
        s = fromIntegral $ log2 (r `div` fx)

invF2M :: Integer -> Integer -> Integer
invF2M fx n = go n fx 1 0
    where
      go u v g1 g2
          | u == 1 = modF2M fx g1
          | otherwise = if j < 0
                           then go u (v `xor` shift u (-j)) g1 (g2 `xor` shift g1 (-j))
                           else go (u `xor` shift v j) v (g1 `xor` shift g2 j) g2
        where
          j = fromIntegral $ log2 u - log2 v

log2 :: Integer -> Integer
log2 = imLog 2
{-# INLINE log2 #-}

-- http://www.haskell.org/pipermail/haskell-cafe/2008-February/039465.html
imLog :: Integer -> Integer -> Integer
imLog b x = if x < b then 0 else (x `div` b^l) `doDiv` l
  where
    l = 2 * imLog (b * b) x
    doDiv x' l' = if x' < b then l' else (x' `div` b) `doDiv` (l' + 1)
{-# INLINE imLog #-}
