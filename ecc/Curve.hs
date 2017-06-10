{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Curve where


import Protolude

{-

Short Weierstrass curves

y^2 = x^3 + ax + b (mod p)

Reference: https://www.hyperelliptic.org/EFD/g1p/auto-shortw.html

-}

data Q
type Fq = Ring Q

-- | prime field modulus
p :: Integer
p = 115792089237316195423570985008687907853269984665640564039457584007908834671663

-- | order
n :: Integer
n = 115792089237316195423570985008687907852837564279074904382605163141518161494337

-- | base point
g :: Point
g = affinePoint
  0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
  0X483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8

-- | curve coefficent
coeffB :: Fq
coeffB = 7

-- | curve coefficent
coeffA :: Fq
coeffA = 0

-------------------------------------------------------------------------------
-- Ring Structure
-------------------------------------------------------------------------------

newtype Ring n = Ring Integer
    deriving (Show, Eq, Ord)

instance Num (Ring a) where
  fromInteger n = Ring (n `mod` p)
  (Ring x) + (Ring y) = fromInteger (x+y)
  (Ring x) * (Ring y) = fromInteger (x*y)
  negate (Ring x) = fromInteger $ negate x
  signum (Ring x) = fromInteger $ signum x
  abs n = n

instance Real (Ring n) where
  toRational (Ring i) = toRational i

instance Integral (Ring n) where
  (Ring x) `quot` (Ring y) = fromInteger $ x `quot` y
  (Ring x) `rem` (Ring y) = fromInteger $ x `rem` y
  (Ring x) `div` (Ring y) = fromInteger $ x `div` y
  (Ring x) `mod` (Ring y) = fromInteger $ x `mod` y
  (Ring x) `quotRem` (Ring y) = (fromInteger a, fromInteger b)
      where (a,b) = x `quotRem` y
  (Ring x) `divMod` (Ring y) = (fromInteger a, fromInteger b)
      where (a,b) = x `divMod` y
  toInteger (Ring x) = x

instance Bounded (Ring n) where
  minBound = 0
  maxBound = -1

instance Enum (Ring n) where
  succ r@(Ring i)
    | r == maxBound = panic "Invalid succ"
    | otherwise = fromInteger $ succ i
  pred r@(Ring i) 
    | r == minBound = panic "Invalid pred"
    | otherwise = fromInteger $ pred i
  toEnum i
    | toInteger i >= toInteger (minBound :: Ring n) && 
      toInteger i <= toInteger (maxBound :: Ring n) = r
    | otherwise = panic "Invalid toEnum"
    where r = fromInteger $ toEnum i
  fromEnum (Ring i) = fromEnum i

instance Fractional (Ring a) where
  recip (Ring a) = fromInteger $ mulInverse a p
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- | Extended Euclidean algorithm
euclidean :: Integer -> Integer -> Integer -> (Integer, Integer)
euclidean a b p
  | b == 0 = (1,0)
  | otherwise = (t, (s - q*t) `mod` p)
  where (q,r) = quotRem a b
        (s,t) = euclidean b r p

-- | Calculate the inverse modulo p
mulInverse :: Integer -> Integer -> Integer
mulInverse a p 
  | a*s `mod` p == 1 = s
  | otherwise = panic "No multiplicative inverse"
  where (s,_) = euclidean a p p

quadraticResidue :: Fq -> [Fq]
quadraticResidue x = guard (y^2 == x) >> [y, (-y)]
  where 
    q = (p + 1) `div` 4
    y = x^q

-------------------------------------------------------------------------------
-- Points
-------------------------------------------------------------------------------

data Point 
  = Point !Fq !Fq !Fq
  | Infinity
  deriving Show

instance Eq Point where
  Infinity == Infinity = True
  (Point x1 y1 z1) == (Point x2 y2 z2) = a == b && c == d
    where 
      a = x1*z2 ^ 2
      b = x2*z1 ^ 2
      c = y1*z2 ^ 3
      d = y2*z1 ^ 3
  _ == _ = False

getX :: Point -> Maybe Fq
getX point = fst <$> (fromJacobian point)

getY :: Point -> Maybe Fq
getY point = snd <$> (fromJacobian point)

-- | Cnstruct Jacobian point from affine coordinates
affinePoint :: Fq -> Fq -> Point
affinePoint x y = Point x y 1

-- | Convert Jacobian to Affine point
fromJacobian :: Point -> Maybe (Fq, Fq)
fromJacobian = \case
  Infinity    -> Nothing
  Point _ _ 0 -> Nothing
  Point x y z -> Just (x/z^2, y/z^3)

isInfinity :: Point -> Bool
isInfinity = \case
  Infinity    -> True
  Point _ _ 0 -> True
  _           -> False

validatePoint :: Point -> Bool
validatePoint point = case fromJacobian point of
  Nothing    -> False 
  Just (x,y) -> y ^ 2 == x ^ 3 + coeffB

addition :: Point -> Point -> Point
addition Infinity point = point
addition point Infinity = point
addition p1@(Point x1 y1 z1) (Point x2 y2 z2)
  | u1 == u2 = if s1 == s2 then double p1 else Infinity
  | otherwise = Point x3 y3 z3
  where 
    u1 = x1*z2 ^ 2
    u2 = x2*z1 ^ 2
    s1 = y1*z2 ^ 3
    s2 = y2*z1 ^ 3
    h  = u2 - u1
    r  = s2 - s1
    x3 = r ^ 2 - h ^ 3 - 2*u1*h ^ 2
    y3 = r*(u1 * h ^ 2 - x3) - s1 * h ^ 3
    z3 = h * z1 * z2

double :: Point -> Point
double Infinity = Infinity
double (Point x y z)
  | y == 0 = Infinity
  | otherwise = Point x' y' z'
  where 
    s  = 4*x*y ^ 2
    m  = 3*x ^ 2
    x' = m ^ 2 - 2*s
    y' = m*(s - x') - 8*y ^ 4
    z' = 2*y*z

neg :: Point -> Point
neg (Point x y z) = Point x (-y) z

multiply pt 0 = Infinity
multiply pt 1 = pt
multiply pt n
  | even n    = multiply (double pt) (n `div` 2)
  | otherwise = addition (multiply (double pt) (n `div` 2)) pt

class Group a where
  mult :: a -> a -> a
  inverse :: a -> a
  unit :: a

instance Group Point where
  mult = addition
  unit = Infinity
  inverse = neg

-------------------------------------------------------------------------------
-- Keys
-------------------------------------------------------------------------------

newtype PrivateKey = PrivateKey Integer
newtype PublicKey  = PublicKey Point

pubKey :: PrivateKey -> PublicKey
pubKey (PrivateKey k) = PublicKey (multiply g k)

-------------------------------------------------------------------------------
-- ECDSA
-------------------------------------------------------------------------------

data Signature = Signature Integer Integer
  deriving (Show)

sign
  :: Integer    -- ^ nonce
  -> PrivateKey -- ^ private key
  -> Integer    -- ^ hash of msg
  -> Maybe Curve.Signature
sign k (PrivateKey d) z = do
  let point = multiply g k
  Ring r <- getX point
  let kInv = mulInverse k n
  let s = kInv * (z + r * d) `mod` n
  when (r == 0 || s == 0) Nothing
  return $ Signature r s

verify 
  :: PublicKey
  -> Signature
  -> Integer
  -> Bool
verify (PublicKey q) (Signature r s) z
  | r < 1 || r >= n || s < 1 || s >= n = False
  | otherwise = maybe False (r ==) $ do
      w <- pure $ mulInverse s n
      let u1 = z * w `mod` n
          u2 = r * w `mod` n
          x = (multiply g u1) `addition` (multiply q u2)
      case x of
        Infinity -> Nothing
        Point (Ring x1) _ _ -> pure x1
