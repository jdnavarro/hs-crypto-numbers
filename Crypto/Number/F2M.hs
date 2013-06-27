{-# LANGUAGE BangPatterns #-}
module Crypto.Number.F2M
    ( PolyBin(..)
    , fromList
    , toInteg
    -- * F2M arithmetic operations over integers
    , addF2M
    , mulF2M
    , modF2M
    , invF2M
    -- * Arithmetic operations for binary polynomials in F2M
    -- , mulPolyF2M
    -- , mulPolyF2M'
    -- , invPolyF2M
    -- * Arithmetic operations for binary polynomials
    -- , addPoly
    -- , mulPoly
    -- , modPoly
    -- , invPoly
    ) where

import Data.Bits ((.&.),xor,shift,clearBit)
import Data.List (elemIndices, intercalate, sortBy)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

newtype PolyBin = PolyBin (Vector Int) deriving (Eq)

instance Show PolyBin where
    show (PolyBin v) = intercalate "+" $ map (("x^" ++) . show) $ V.toList v

fromInteg :: Integer -> PolyBin
fromInteg n = PolyBin $ V.fromList $ map (m-) $ elemIndices '1' s
  where
    s = showIntAtBase 2 intToDigit n []
    m = length s - 1

fromList :: [Int] -> PolyBin
fromList = PolyBin . V.fromList . sortBy (flip compare)

toInteg :: PolyBin -> Integer
toInteg (PolyBin v) = V.sum $ V.map (2^) v

{-------------
-  Integer  -
-------------}

addF2M :: Integer -> Integer -> Integer
addF2M = xor

-- http://pythonfiddle.com/binary-finite-field-multiplication/
mulF2M :: Integer -> Integer -> Integer -> Integer
mulF2M fx = go 0
  where
    go c p1 p2 | p2 > 0 = go (if (p2 .&. 1) /= 0
                                 then c `xor` p1
                                 else c)
                             (if (shift p1 1 .&. mask1) /= 0
                                 then shift p1 1 `xor` polyRed
                                 else shift p1 1)
                             (shift p2 (-1))
               | otherwise = c .&. mask2

    mask1 :: Integer
    mask1 = 2 ^ imLog 2 fx
    mask2 :: Integer
    mask2 = mask1 - 1
    polyRed = clearBit fx (fromIntegral $ imLog 2 fx)


modF2M :: Integer -> Integer -> Integer
modF2M fx = go
  where
    go !r | s < 0  = r
          | otherwise = go $ r `xor` shift fx s
      where
        s = fromIntegral $ imLog 2 r - imLog 2 fx

invF2M :: Integer -> Integer -> Integer
invF2M fx n = toInteg $ invPolyF2M (fromInteg fx) (fromInteg n)

{--------------
-  Poly F2M  -
--------------}

invPolyF2M :: PolyBin -> PolyBin -> PolyBin
invPolyF2M fx p = modPoly fx $ invPoly fx p

{----------
-  Poly  -
----------}

addPoly :: PolyBin -> PolyBin -> PolyBin
addPoly p1 p2 = fromInteg $ toInteg p1 `xor` toInteg p2

modPoly :: PolyBin -> PolyBin -> PolyBin
modPoly fx = go
  where
    m = weight fx
    go !r | m <= w = go $ r `addPoly` mul (w - m) fx
          | otherwise = r
      where
        w = weight r

invPoly :: PolyBin -> PolyBin -> PolyBin
invPoly fx p = loop p fx (fromInteg 1) (fromInteg 0)
  where
    loop !u !v !g1 !g2
        | u == fromInteg 1 = g1
        | otherwise =
            let j = weight u - weight v
            in if j < 0 then loop u (v `addPoly` mul (-j) u) g1 (g2 `addPoly` mul (-j) g1)
                        else loop (u `addPoly` mul j v) v (g1 `addPoly` mul j g2) g2

{---------
-  aux  -
---------}

mul :: Int -> PolyBin -> PolyBin
mul j (PolyBin v) = PolyBin $ V.map (j+) v

weight :: PolyBin -> Int
weight (PolyBin v) = fromMaybe 0 $ v !? 0

-- http://www.haskell.org/pipermail/haskell-cafe/2008-February/039465.html
imLog :: Integer -> Integer -> Integer
imLog b x = if x < b then 0 else (x `div` b^l) `doDiv` l
  where
    l = 2 * imLog (b * b) x
    doDiv !x' !l' = if x' < b then l' else (x' `div` b) `doDiv` (l' + 1)

