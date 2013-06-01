module Crypto.Number.PolynomialBin
    ( PolynomialBin
    , toList
    , fromList
    , toPolynomial
    , fromPolynomial
    ) where

import Data.List (sort)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Crypto.Number.Polynomial (Monomial(..), Polynomial)
import qualified Crypto.Number.Polynomial as P

newtype PolynomialBin = PolynomialBin (Vector Int)

toList :: PolynomialBin -> [Int]
toList (PolynomialBin v) = V.toList v

fromList :: [Int] -> PolynomialBin
fromList = PolynomialBin . V.fromList . reverse . sort . filter (/= 0)

toPolynomial :: PolynomialBin -> Polynomial
toPolynomial = P.fromList . map (`Monomial` 1) . toList

fromPolynomial :: Polynomial -> PolynomialBin
fromPolynomial = fromList . map (\(Monomial w _) -> w)
                          . filter (\(Monomial _ n) -> odd n)
                          . P.toList
