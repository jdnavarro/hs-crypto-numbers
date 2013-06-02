module Crypto.Number.PolynomialBin
    -- * binary polynomial operations
    ( PolynomialBin
    , toList
    , fromList
    , toPolynomial
    , fromPolynomial
    , mulPolyBin
    , addPolyBin
    , reducePolyBin
    -- * binary polynomial operations as integers
    , addition
    , multiplication
    ) where

import Data.Bits (xor)
import Data.List (sort, elemIndices, intercalate)
import Numeric (readInt, showIntAtBase)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Crypto.Number.Polynomial (Monomial(..), Polynomial, mulPoly)
import qualified Crypto.Number.Polynomial as P

newtype PolynomialBin = PolynomialBin (Vector Int) deriving (Eq,Ord)

instance Show PolynomialBin where
    show (PolynomialBin v) = intercalate "+" $ map (("x^" ++) . show) $ V.toList v

toList :: PolynomialBin -> [Int]
toList (PolynomialBin v) = V.toList v

fromList :: [Int] -> PolynomialBin
fromList = PolynomialBin . V.fromList . reverse . sort

-- TODO: is it worth Integral instance?
toInteg :: PolynomialBin -> Integer
toInteg (PolynomialBin v) =
    readBin . reverse $ map (\n -> if n `V.elem` v then '1' else '0') [0 .. V.head v]

-- TODO: is it worth Num instance?
fromInteg :: Integer -> PolynomialBin
fromInteg n = fromList . map (m-) $ elemIndices '1' s
  where s = showBin n
        m = length s - 1

toPolynomial :: PolynomialBin -> Polynomial
toPolynomial = P.fromList . map (`Monomial` 1) . toList

fromPolynomial :: Polynomial -> PolynomialBin
fromPolynomial = fromList . map (\(Monomial w _) -> w)
                          . filter (\(Monomial _ n) -> odd n)
                          . P.toList

addPolyBin :: PolynomialBin -> PolynomialBin -> PolynomialBin
addPolyBin p1 p2 = fromInteg $ toInteg p1 `xor` toInteg p2

mulPolyBin :: PolynomialBin -> PolynomialBin -> PolynomialBin
mulPolyBin p1 p2 = fromPolynomial $ toPolynomial p1 `mulPoly` toPolynomial p2

reducePolyBin :: Int -> PolynomialBin -> PolynomialBin -> PolynomialBin
reducePolyBin m fx@(PolynomialBin v0) p@(PolynomialBin v)
    | m < n = reducePolyBin m fx
            $ p `addPolyBin` (PolynomialBin $ V.map ((n - m) +) v0)
    | otherwise = p
  where
    n = V.head v

readBin :: String -> Integer
readBin = fst . head . readInt 2 (`elem` "01") (\c -> if c == '1' then 1 else 0)

showBin :: Integer -> String
showBin n = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') n ""

-----

addition :: Integer -> Integer -> Integer
addition n1 n2 = n1 `xor` n2

multiplication :: Integer -> Integer -> Integer
multiplication n1 n2 = toInteg $ fromInteg n1 `mulPolyBin` fromInteg n2
