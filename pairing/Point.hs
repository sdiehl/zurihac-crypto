{-# LANGUAGE DeriveFunctor #-}

module Point (
  Point(..),
  pointY,
  pointX,
  gDouble,
  gAdd,
  gNeg,
  gMul,
) where

import Protolude

data Point a
  = Point a a
  | Infinity
  deriving (Eq, Ord, Show, Functor)

pointX :: Point a -> a
pointX (Point x _) = x

pointY :: Point a -> a
pointY (Point _ y) = y

instance (Fractional t, Eq t) => Num (Point t) where
  (+)         = gAdd
  negate      = gNeg
  (*)         = notImplemented
  abs         = notImplemented
  signum      = notImplemented
  fromInteger = notImplemented

-- | Addition
gAdd
  :: (Fractional t, Eq t)
  => Point t
  -> Point t
  -> Point t
gAdd Infinity a = Infinity
gAdd a Infinity = Infinity
gAdd (Point x1 y1) (Point x2 y2)
  | x2 == x1 && y2 == y1 = gDouble (Point x1 y1)
  | x2 == x1             = Infinity
  | otherwise            = Point x' y'
  where
    l = (y2 - y1) / (x2 - x1)
    x' = l^2 - x1 - x2
    y' = -l * x' + l * x1 - y1

-- | Doubling
gDouble :: (Fractional t, Eq t) => Point t -> Point t
gDouble Infinity = Infinity
gDouble (Point x 0) = Infinity
gDouble (Point x y) = Point x' y'
  where
    l = 3*x^2 / (2*y)
    x' = l^2 - 2*x
    y' = -l * x' + l * x - y

-- | Negation
gNeg
  :: (Fractional t, Eq t)
  => Point t
  -> Point t
gNeg Infinity = Infinity
gNeg (Point x y) = Point x (-y)


-- | Multiplication
gMul
  :: (Eq t, Integral a, Fractional t)
  => Point t
  -> a
  -> Point t
gMul pt 0 = Infinity
gMul pt 1 = pt
gMul pt n
  | even n    = gMul (gDouble pt) (n `div` 2)
  | otherwise = gAdd (gMul (gDouble pt) (n `div` 2)) pt
