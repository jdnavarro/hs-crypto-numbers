{-| This module provides basic arithmetic operations over F₂m.

    The 'm' parameter is implicitly derived from the irreducible polynomial
    where applicable.
-}
{-# LANGUAGE MagicHash #-}
module Crypto.Number.F2M
    ( addF2M
    , mulF2M
    , squareF2M
    , modF2M
    , invF2M
    , divF2M
    ) where

import Control.Applicative ((<$>))
import GHC.Exts
import GHC.Integer.Logarithms (integerLog2#)
import Data.Bits ((.&.),(.|.),xor,shift,testBit)

-- | Addition over F₂m. This is just a synonym of  'xor'.
addF2M :: Integer -> Integer -> Integer
addF2M = xor
{-# INLINE addF2M #-}

-- | Binary polynomial reduction modulo using long division algorithm.
modF2M :: Integer  -- ^ Irreducible binary polynomial
       -> Integer -> Integer
modF2M fx = go
  where
    go n | s == 0  = n `xor` fx
         | s < 0 = n
         | otherwise = go $ n `xor` shift fx s
      where
        s = log2 n - log2 fx
{-# INLINE modF2M #-}

-- | Multiplication over F₂m.
mulF2M :: Integer  -- ^ Irreducible binary polynomial
       -> Integer -> Integer -> Integer
mulF2M fx n1 n2 = modF2M fx
                $ go (if n2 `mod` 2 == 1 then n1 else 0) (log2 n2)
  where
    go n s | s == 0  = n
           | otherwise = if testBit n2 s
                            then go (n `xor` shift n1 s) (s - 1)
                            else go n (s - 1)

-- | Squaring over F₂m.
-- TODO: This is still slower than @mulF2m@.

-- Multiplication table? C?
squareF2M :: Integer  -- ^ Irreducible binary polynomial
          -> Integer -> Integer
squareF2M fx = modF2M fx . square

square :: Integer -> Integer
square n1 = go n1 ln1
  where
    ln1 = log2 n1
    go n s | s == 0 = n
           | otherwise = go (x .|. y) (s - 1)
      where
        x = shift (shift n (2 * (s - ln1) - 1)) (2 * (ln1 - s) + 2)
        y = n .&. (shift 1 (2 * (ln1 - s) + 1) - 1)
{-# INLINE square #-}

-- | Inversion over  F₂m using extended Euclidean algorithm.
invF2M :: Integer -- ^ Irreducible binary polynomial
       -> Integer -> Maybe Integer
invF2M fx n = go n fx 1 0
    where
      go u v g1 g2
          | u == 0 = Nothing
          | u == 1 = Just $ modF2M fx g1
          | otherwise = if j < 0
                           then go u  (v  `xor` shift  u (-j))
                                   g1 (g2 `xor` shift g1 (-j))
                           else go (u  `xor` shift v  j) v
                                   (g1 `xor` shift g2 j) g2
        where
          j = log2 u - log2 v

-- | Division over F₂m. If the dividend doesn't have an inverse it returns
-- 'Nothing'.
divF2M :: Integer  -- ^ Irreducible binary polynomial
       -> Integer  -- ^ Dividend
       -> Integer  -- ^ Quotient
       -> Maybe Integer
divF2M fx n1 n2 = mulF2M fx n1 <$> invF2M fx n2


log2 :: Integer -> Int
log2 0 = 0
log2 x = I# (integerLog2# x)
{-# INLINE log2 #-}
