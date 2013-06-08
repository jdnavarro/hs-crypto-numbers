module Crypto.Number.F2
    -- * F2m arithmetic operations over integers
    ( padd
    , pmod
    , pmul
    , pinv
    -- * Arithmetic operations for F2m polynomials
    , mulF2m
    , modF2m
    , invF2m
    -- * Arithmetic operations for F2 polynomials
    , addF2
    , mulF2
    , invF2
    ) where

import Control.Applicative (liftA2)
import Data.Bits (xor)
import Data.List (elemIndices, intercalate, group, sort)
import Data.Char (intToDigit)
import Data.Maybe (fromMaybe)
import Numeric (showIntAtBase)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V

newtype F2 = F2 (Vector Int) deriving (Eq,Ord)

instance Show F2 where
    show (F2 v) = intercalate "+" $ map (("x^" ++) . show) $ V.toList v

fromInteg :: Integer -> F2
fromInteg n = F2 $ V.fromList $ map (m-) $ elemIndices '1' s
  where
    s = showIntAtBase 2 intToDigit n []
    m = length s - 1

toInteg :: F2 -> Integer
toInteg (F2 v) = V.sum $ V.map (2^) v

padd :: Integer -> Integer -> Integer
padd = xor

pmul :: Int -> Integer -> Integer -> Integer -> Integer
pmul m nx n1 n2 =
    toInteg $ modF2m m (fromInteg nx) $ fromInteg n1 `mulF2` fromInteg n2

pmod :: Int -> Integer -> Integer -> Integer
pmod m nx n = toInteg $ modF2m m (fromInteg nx) (fromInteg n)

pinv :: Int -> Integer -> Integer -> Integer
pinv m fx p = toInteg $ invF2m m (fromInteg fx) (fromInteg p)

mulF2m :: Int -> F2 -> F2 -> F2 -> F2
mulF2m m fx f1 f2 = modF2m m fx $ mulF2 f1 f2

modF2m :: Int -> F2 -> F2 -> F2
modF2m m fx p
    | m <= w = modF2m m fx $ p `addF2` mul (w - m) fx
    | otherwise = p
  where
    w = weight p

invF2m :: Int -> F2 -> F2 -> F2
invF2m m fx p = modF2m m fx $ invF2 fx p

addF2 :: F2 -> F2 -> F2
addF2 f1 f2 = fromInteg $ toInteg f1 `xor` toInteg f2

mulF2 :: F2 -> F2 -> F2
mulF2 (F2 v1) (F2 v2) =
    F2 . V.fromList . reverse . map head . filter (odd . length) . group . sort
       $ liftA2 (+) (V.toList v1) (V.toList v2)

invF2 :: F2 -> F2 -> F2
invF2 fx p = loop p fx (fromInteg 1) (fromInteg 0)
  where
    loop u v g1 g2
        | u == fromInteg 1 = g1
        | otherwise =
            let j = weight u - weight v
            in if j < 0 then loop u (v `addF2` mul (-j) u) g1 (g2 `addF2` mul (-j) g1)
                        else loop (u `addF2` mul j v) v (g1 `addF2` mul j g2) g2

mul :: Int -> F2 -> F2
mul j (F2 v) = F2 $ V.map (j+) v

weight :: F2 -> Int
weight (F2 v) = fromMaybe 0 $ v !? 0
