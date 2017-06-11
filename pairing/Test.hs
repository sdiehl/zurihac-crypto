{-# LANGUAGE NoImplicitPrelude #-}

module Test where

import Protolude

import Fq
import Fq2
import Fq6
import Fq12
import Point
import Group

import Test.QuickCheck

import Text.Show.Pretty

ppDump :: Show a => a -> IO ()
ppDump a = putStrLn (ppShow a :: [Char])

-------------------------------------------------------------------------------
-- Generators
-------------------------------------------------------------------------------

instance Arbitrary Fq where
  arbitrary = Fq.new <$> arbitrary

instance Arbitrary Fq2 where
  arbitrary = Fq2 <$> arbitrary <*> arbitrary

instance Arbitrary Fq6 where
  arbitrary = Fq6 
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Fq12 where
  arbitrary = Fq12 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Point a) where
  arbitrary = Point <$> arbitrary <*> arbitrary

size :: Int
size = 1000

-------------------------------------------------------------------------------
-- Fq
-------------------------------------------------------------------------------

testCommMul :: IO ()
testCommMul = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq -> Fq -> Bool
    f a b = a * b == b * a

testCommAdd :: IO ()
testCommAdd = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq -> Fq -> Bool
    f a b = a + b == b + a

testAssocAdd :: IO ()
testAssocAdd = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq -> Fq -> Fq -> Property
    f a b c = c /= 0 ==> ((a+b) / c) == (a/c + b/c)

testAssocMul :: IO ()
testAssocMul = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq -> Fq -> Fq -> Bool
    f a b c = ((a+b) * c) == a*c + b*c

fq2Assoc :: IO ()
fq2Assoc = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq2 -> Fq2 -> Fq2 -> Property
    f a b c = c /= 0 ==> ((a+b) / c) == (a/c + b/c)

-------------------------------------------------------------------------------
-- Fq2
-------------------------------------------------------------------------------

-- (a + bi)^p = a + bi if p % 4 = 1
-- (a + bi)^p = a - bi if p % 4 = 3
fq2Pow :: IO ()
fq2Pow = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq2 -> Integer -> Bool
    f n@(Fq2 a b) p 
      | p `mod` 4 == 1 = n^p == Fq2 a b
      | p `mod` 4 == 3 = n^p == Fq2 a (-b)
      | otherwise      = True

-- x * x^-1 = 1
fq2MulInv :: IO ()
fq2MulInv = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq2 -> Property
    f a = a /= 0 ==> a * fq2inv a == fq2one

-- x + -x = 0
fq2AddInv :: IO ()
fq2AddInv = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq2 -> Property
    f a = a /= 0 ==> a + (negate a) == fq2zero

-- (x+y) * z = x*z + y*z
fq2DistMul :: IO ()
fq2DistMul = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq2 -> Fq2 -> Fq2 -> Bool
    f a b c = ((a+b) * c) == a*c + b*c

-------------------------------------------------------------------------------
-- Fq6
-------------------------------------------------------------------------------

fq6SubAnticommm :: IO ()
fq6SubAnticommm = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Fq6 -> Bool
    f a b = (a - b) == negate (b - a)

fq6MulComm :: IO ()
fq6MulComm = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Fq6 -> Bool
    f a b = a * b == b * a

fq6DistMul :: IO ()
fq6DistMul = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Fq6 -> Fq6 -> Bool
    f a b c = a * (b + c) == a * b + a * c

fq6MulAssoc :: IO ()
fq6MulAssoc = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Fq6 -> Fq6 -> Bool
    f a b c = a * (b * c) == (a * b) * c

fq6SubIdentity :: IO ()
fq6SubIdentity = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Bool
    f a = a - 0 == a

fq6SubInverse :: IO ()
fq6SubInverse = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Bool
    f a = a - a == 0

fq6MulInverse :: IO ()
fq6MulInverse = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq6 -> Property
    f a = a /= 0 ==> a * fq6inv a == fq6one

-------------------------------------------------------------------------------
-- FQ12
-------------------------------------------------------------------------------

fq12MulAssoc :: IO ()
fq12MulAssoc = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq12 -> Fq12 -> Fq12 -> Bool
    f a b c = a * (b * c) == (a * b) * c

fq12DistMul :: IO ()
fq12DistMul = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq12 -> Fq12 -> Fq12 -> Bool
    f a b c = a * (b + c) == a * b + a * c

fq12MulComm :: IO ()
fq12MulComm = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq12 -> Fq12 -> Bool
    f a b = a * b == b * a

fq12MulInverse :: IO ()
fq12MulInverse = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Fq12 -> Property
    f a = a /= 0 ==> a * fq12inv a == fq12one

-------------------------------------------------------------------------------
-- G1
-------------------------------------------------------------------------------

groupAdd :: IO ()
groupAdd = quickCheckWith stdArgs { maxSuccess = size } f
  where
    f :: Point Fq -> Point Fq -> Point Fq -> Property
    f a b c = c /= (Point 0 0) ==> gAdd (gAdd a b) c == gAdd a (gAdd b c)

-------------------------------------------------------------------------------
-- Test Suite
-------------------------------------------------------------------------------

testAll :: IO ()
testAll = sequence_ [
  -- Fq
    putStrLn "FP"
  , testCommMul 
  , testCommAdd 
  , testAssocAdd
  , testAssocMul

  -- Fq2
  , putStrLn "Fq2"
  , fq2Assoc
  , fq2DistMul
  , fq2MulInv
  , fq2AddInv

  -- Fq6
  , putStrLn "Fq6"
  , fq6SubAnticommm
  , fq6MulComm
  , fq6DistMul
  , fq6MulAssoc
  , fq6SubIdentity
  , fq6SubInverse
  , fq6MulInverse

  -- Fq12
  , putStrLn "FP12"
  , fq12MulInverse
  , fq12MulAssoc
  , fq12DistMul
  , fq12MulComm

  {-, groupAdd-}
  ]
