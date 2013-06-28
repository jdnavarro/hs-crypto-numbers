{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.QuickCheck
import Test.HUnit
--import Test.QuickCheck.Test

import Control.Applicative ((<$>))

import qualified Data.ByteString as B
import Data.ByteString.Char8 () -- orphan IsString instance

import Crypto.Number.ModArithmetic
import Crypto.Number.Basic
import Crypto.Number.Generate
import Crypto.Number.Prime
import Crypto.Number.Serialize
import Crypto.Number.F2M

import RNG

prop_gcde_binary_valid :: (Positive Integer, Positive Integer) -> Bool
prop_gcde_binary_valid (Positive a, Positive b) =
    and [v==v', a*x' + b*y' == v', a*x + b*y == v, gcd a b == v]
    where (x,y,v)    = gcde_binary a b
          (x',y',v') = gcde a b

prop_modexp_rtl_valid :: (NonNegative Integer,
                          NonNegative Integer,
                          Positive Integer)
                      -> Bool
prop_modexp_rtl_valid (NonNegative a, NonNegative b, Positive m) =
    exponantiation_rtl_binary a b m == ((a ^ b) `mod` m)

prop_modinv_valid :: (Positive Integer, Positive Integer) -> Bool
prop_modinv_valid (Positive a, Positive m)
    | m > 1           = case inverse a m of
                             Just ainv -> (ainv * a) `mod` m == 1
                             Nothing   -> True
    | otherwise       = True

prop_sqrti_valid :: Positive Integer -> Bool
prop_sqrti_valid (Positive i) = l*l <= i && i <= u*u where (l, u) = sqrti i

prop_generate_prime_valid :: Seed -> Bool
prop_generate_prime_valid i =
    -- because of the next naive test, we can't generate easily number above 32 bits
    -- otherwise it slows down the tests to uselessness. when AKS or ECPP is implemented
    -- we can revisit the number here
    primalityTestNaive $ withRNG i (\g -> generatePrime g 32)

prop_miller_rabin_valid :: (Seed, PositiveSmall) -> Bool
prop_miller_rabin_valid (seed, PositiveSmall i)
    | i <= 3    = True
    | otherwise =
        -- miller rabin only returns with certitude that the integer is composite.
        let b = withRNG seed (\g -> isProbablyPrime g i)
         in (not b && not (primalityTestNaive i)) || b

prop_generate_valid :: (Seed, Positive Integer) -> Bool
prop_generate_valid (seed, Positive h) =
    let v = withRNG seed (\g -> generateMax g h)
     in (v >= 0 && v < h)

prop_invF2M_valid :: (Fx, Positive Integer) -> Bool
prop_invF2M_valid (Fx fx, Positive a) = mulF2M fx a (invF2M fx a) == 1

withAleasInteger :: Rng -> Seed -> (Rng -> (a,Rng)) -> a
withAleasInteger g (Seed i) f = fst $ f $ reseed (i2osp $ fromIntegral i) g

withRNG :: Seed -> (Rng -> (a,Rng)) -> a
withRNG = withAleasInteger rng

newtype PositiveSmall = PositiveSmall Integer
                      deriving (Show,Eq)

instance Arbitrary PositiveSmall where
    arbitrary = PositiveSmall . fromIntegral <$> resize (2^(20 :: Int)) (arbitrary :: Gen Int)

data Range = Range Integer Integer
           deriving (Show,Eq)

instance Arbitrary Range where
    arbitrary = do (Positive x) <- arbitrary :: Gen (Positive Int)
                   (Positive r) <- arbitrary :: Gen (Positive Int)
                   return $ Range (fromIntegral x) (fromIntegral r)

newtype Seed = Seed Integer
             deriving (Eq)

instance Show Seed where
    show s = "Seed " ++ show s

instance Arbitrary Seed where
    arbitrary = arbitrary >>= \(Positive i) -> return (Seed i)

data Fx = Fx Integer deriving (Show)

instance Arbitrary Fx where
    -- Rijndael and SEC2 Fx
    arbitrary = elements [Fx 11692013098647223345629478661730264157247460344009] -- x^163+x^7+x^6+x^3+1
                         -- [ [8,4,3,1,0]
                         -- , [163,7,6,3,0]
                         -- , [233,74,0]
                         -- , [239,36,0]
                         -- , [239,158,0]
                         -- , [283,12,7,5,0]
                         -- , [409,87,0]
                         -- , [571,10,5,2,0]
                         -- ]

serializationKATTests = concatMap f vectors
    where f (v, bs) = [ testCase ("i2osp " ++ show v) (i2osp v  @=? bs)
                      , testCase ("os2ip " ++ show v) (os2ip bs @=? v)
                      ]
          vectors =
            [ (0x10000, "\SOH\NUL\NUL")
            , (0x1234, "\DC24")
            , (0xf123456, "\SI\DC24V")
            , (0xf21908421feabd21490, "\SI!\144\132!\254\171\210\DC4\144")
            , (0x7521908421feabd21490, "u!\144\132!\254\171\210\DC4\144")
            ]

main :: IO ()
main = defaultMain
    [ testGroup "serialization"
        [ testProperty "unbinary.binary==id" (\(Positive i) -> os2ip (i2osp i) == i)
        , testProperty "length integer" (\(Positive i) -> B.length (i2osp i) == lengthBytes i)
        , testGroup "KAT" serializationKATTests
        ]
    , testGroup "gcde binary"
        [ testProperty "gcde" prop_gcde_binary_valid
        ]
    , testGroup "exponantiation"
        [ testProperty "right-to-left" prop_modexp_rtl_valid
        ]
    , testGroup "inverse"
        [ testProperty "inverse" prop_modinv_valid
        ]
    , testGroup "sqrt integer"
        [ testProperty "sqrt" prop_sqrti_valid
        ]
    , testGroup "generation"
        [ testProperty "max" prop_generate_valid
        --, testProperty "between" (\seed (Range l h) -> let generated = withRNG seed (\rng -> generateBetween rng l (l+h))
        --                                                in (generated > l && generated < h))
        ]
    , testGroup "primality test"
        [ testProperty "miller-rabin" prop_miller_rabin_valid
        ]
    , testGroup "F2M"
        [ testProperty "invF2M" prop_invF2M_valid
        ]
    ]
