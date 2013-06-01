module Crypto.Number.PolynomialBin
    ( PolynomialBin
    , toList
    , fromList
    , toPolynomial
    , fromPolynomial
    , mulPolyBin
    , addPolyBin
    ) where

import Data.Bits (xor)
import Data.List (sort, elemIndices)
import Numeric (readInt, showIntAtBase)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Crypto.Number.Polynomial (Monomial(..), Polynomial, mulPoly)
import qualified Crypto.Number.Polynomial as P

newtype PolynomialBin = PolynomialBin (Vector Int) deriving (Show)

toList :: PolynomialBin -> [Int]
toList (PolynomialBin v) = V.toList v

fromList :: [Int] -> PolynomialBin
fromList = PolynomialBin . V.fromList . reverse . sort

toInt :: PolynomialBin -> Int
toInt (PolynomialBin v) =
    readBin . reverse $ map (\n -> if n `V.elem` v then '1' else '0') [0 .. V.head v]

fromInt :: Int -> PolynomialBin
fromInt n = fromList . map (m-) $ elemIndices '1' s
  where s = showBin n
        m = length s - 1

toPolynomial :: PolynomialBin -> Polynomial
toPolynomial = P.fromList . map (`Monomial` 1) . toList

fromPolynomial :: Polynomial -> PolynomialBin
fromPolynomial = fromList . map (\(Monomial w _) -> w)
                          . filter (\(Monomial _ n) -> odd n)
                          . P.toList

addPolyBin :: PolynomialBin -> PolynomialBin -> PolynomialBin
addPolyBin p1 p2 = fromInt $ toInt p1 `xor` toInt p2

mulPolyBin :: PolynomialBin -> PolynomialBin -> PolynomialBin
mulPolyBin p1 p2 = fromPolynomial $ toPolynomial p1 `mulPoly` toPolynomial p2

readBin :: String -> Int
readBin = fst . head . readInt 2 (`elem` "01") (\c -> if c == '1' then 1 else 0)

showBin :: Int -> String
showBin n = showIntAtBase 2 (\x -> if x == 0 then '0' else '1') n ""
