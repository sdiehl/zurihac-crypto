{-# LANGUAGE DeriveFunctor #-}

module Point (
  Point(..),
  gDouble,
  gAdd,
  gNeg,
) where

import Protolude

data Point a
  = Point a a
  | Infinity
  deriving (Eq, Ord, Show, Functor)

pointX :: Point a -> a
pointX (Point x y) = x

pointY :: Point a -> a
pointY (Point x y) = y

instance (Fractional t, Eq t) => Num (Point t) where
  (+) = gAdd
  negate = gNeg
  (*) = notImplemented
  abs = notImplemented
  signum = notImplemented
  fromInteger = notImplemented


gDouble :: Fractional t => Point t -> Point t
gDouble Infinity = Infinity
gDouble (Point x y) = Point x' y'
  where
    l = 3*x^2 / (2*y)
    x' = l^2 - 2*x
    y' = -l * x' + l * x - y

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

-- | Negation
gNeg  
  :: (Fractional t, Eq t)
  => Point t
  -> Point t
gNeg Infinity = Infinity
gNeg (Point x y) = Point x (-y)
