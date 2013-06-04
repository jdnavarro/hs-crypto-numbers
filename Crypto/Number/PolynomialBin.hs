module Crypto.Number.PolynomialBin
    -- * F2m arithmetic operations over integers
    ( padd
    , pmod
    , pmul
    -- * Arithmetic operations for F2m polynomials
    , modF2m
    , mulF2m
    -- * Arithmetic operations for F2 polynomials
    , addF2
    , mulF2
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

mulF2m :: Int -> F2 -> F2 -> F2 -> F2
mulF2m m fx f1 f2 = modF2m m fx $ mulF2 f1 f2

modF2m :: Int -> F2 -> F2 -> F2
modF2m m fx@(F2 vx) f@(F2 v)
    | m < w = modF2m m fx $ f `addF2` (F2 $ V.map ((w - m) +) vx)
    | otherwise = f
  where
    w = fromMaybe 0 $ v !? 0

addF2 :: F2 -> F2 -> F2
addF2 f1 f2 = fromInteg $ toInteg f1 `xor` toInteg f2

mulF2 :: F2 -> F2 -> F2
mulF2 (F2 v1) (F2 v2) =
    F2 . V.fromList . reverse . map head . filter (odd . length) . group . sort
       $ liftA2 (+) (V.toList v1) (V.toList v2)
