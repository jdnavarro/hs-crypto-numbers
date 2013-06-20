{-# LANGUAGE BangPatterns #-}
module Crypto.Number.F2M
    ( PolyBin
    , fromList
    -- * F2M arithmetic operations over integers
    , addF2M
    , mulF2M
    , modF2M
    , invF2M
    -- * Arithmetic operations for binary polynomials in F2M
    , mulPolyF2M
    , modPolyF2M
    , invPolyF2M
    -- * Arithmetic operations for binary polynomials
    , addPoly
    , mulPoly
    , invPoly
    ) where

import Control.Applicative (liftA2)
import Data.Bits (xor,shift)
import Data.List (elemIndices, intercalate, group, sort, sortBy)
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

addF2M :: Integer -> Integer -> Integer
addF2M = xor

mulF2M :: PolyBin -> Integer -> Integer -> Integer
mulF2M fx n1 n2 =
    toInteg $ modPolyF2M fx $ fromInteg n1 `mulPoly` fromInteg n2

modF2M :: PolyBin -> Integer -> Integer
modF2M fx = go
  where
    x = toInteg fx
    go !r | s < 0  = r
          | otherwise = go $ r `xor` shift x s
      where
        s = fromIntegral $ imLog 2 r - imLog 2 x

invF2M :: PolyBin -> Integer -> Integer
invF2M fx n = toInteg $ invPolyF2M fx (fromInteg n)

mulPolyF2M :: PolyBin -> PolyBin -> PolyBin -> PolyBin
mulPolyF2M fx p1 p2 = modPolyF2M fx $ mulPoly p1 p2

modPolyF2M :: PolyBin -> PolyBin -> PolyBin
modPolyF2M fx = go
  where
    m = weight fx
    go !r | m <= w = go $ r `addPoly` mul (w - m) fx
          | otherwise = r
      where
        w = weight r

invPolyF2M :: PolyBin -> PolyBin -> PolyBin
invPolyF2M fx p = modPolyF2M fx $ invPoly fx p

addPoly :: PolyBin -> PolyBin -> PolyBin
addPoly p1 p2 = fromInteg $ toInteg p1 `xor` toInteg p2

mulPoly :: PolyBin -> PolyBin -> PolyBin
mulPoly (PolyBin v1) (PolyBin v2) =
    PolyBin . V.fromList . reverse . map head . filter (odd . length) . group . sort
       $ liftA2 (+) (V.toList v1) (V.toList v2)

invPoly :: PolyBin -> PolyBin -> PolyBin
invPoly fx p = loop p fx (fromInteg 1) (fromInteg 0)
  where
    loop u v g1 g2
        | u == fromInteg 1 = g1
        | otherwise =
            let j = weight u - weight v
            in if j < 0 then loop u (v `addPoly` mul (-j) u) g1 (g2 `addPoly` mul (-j) g1)
                        else loop (u `addPoly` mul j v) v (g1 `addPoly` mul j g2) g2

mul :: Int -> PolyBin -> PolyBin
mul j (PolyBin v) = PolyBin $ V.map (j+) v

weight :: PolyBin -> Int
weight (PolyBin v) = fromMaybe 0 $ v !? 0

-- Taken from http://www.haskell.org/pipermail/haskell-cafe/2008-February/039465.html
imLog :: Integer -> Integer -> Integer
imLog b x = if x < b then 0 else (x `div` b^l) `doDiv` l
  where
    l = 2 * imLog (b * b) x
    doDiv !x' !l' = if x' < b then l' else (x' `div` b) `doDiv` (l' + 1)
