{-# LANGUAGE OverloadedStrings #-}

module Pairing (
  miller_loop,
  twist1,
  twist2,
  frobex1,
  frobex2,
  toFq12,
) where

import Fq
import Fq2
import Fq12
import Point
import Group
import Params

import Protolude

-------------------------------------------------------------------------------
-- Optimal Ate Pairing
-------------------------------------------------------------------------------

naf :: [Int]
naf = [1, 0, 0, 0, 1, 0, 1, 0, 0, -1, 0, 1, 0, 1, 0, -1, 0, 0, 1, 0, 1,
  0, -1, 0, -1, 0, -1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, -1, 0, 1, 0, 0,
  1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, -1, 0, 0, 0, 1]

miller_loop 
  :: Point Fq12
  -> Point Fq12
  -> Fq12
miller_loop Infinity p = fq12one
miller_loop q Infinity = fq12one
miller_loop q p = finalExp nf
  where
    q1 = frobex1 _q q   -- pi_q(q)
    q2 = frobex2 _q q1  -- pi_{p^2}(q)

    t = q2
    f = fq12one
    
    (nf, nt) = go f t naf

    go :: Fq12 -> Point Fq12 -> [Int] -> (Fq12, Point Fq12)
    go f_ t_ [] = panic "Impossible"
    go f_ t_ (x:[]) = (f_, t_)
    go f_ t_ (x:xs)
      | x == -1 = 
        let
          f'' = f' * linefunc t' (-q) p
          t'' = t' + (-q)
        in
          go f'' t'' xs

      | x == 1  = 
        let
          f'' = f' * linefunc t' q p
          t'' = t' + q
        in
          go f'' t'' xs

      | x == 0  = go f' t' xs
      | otherwise = panic "Impossible" 
      where
        f' = (f_ * f_) * linefunc t_ t_ p
        t' = gDouble t_

-- l_{P1, P2}(T)
linefunc 
  :: (Fractional a, Eq a)
  => Point a -- P1
  -> Point a -- P2
  -> Point a -- T 
  -> a
linefunc (Point x1 y1) (Point x2 y2) (Point xt yt) 
  | x1 /= x2 = 
      let m = (y2 - y1) / (x2 - x1) in
      m * (xt - x1) - (yt - y1)
  | y1 == y2 =
      let m = 3 * x1^2 / (2 * y1) in
      m * (xt - x1) - (yt - y1)
  | otherwise = xt - x1 
  where
    m = (xt - x1) - (yt - y1) -- slope
linefunc _ _ _ = panic "Cannot compute at infinity"

-- | Final exponentiation
finalExp :: Fq12 -> Fq12
finalExp x = x ^ finalExpVal 

-- | Precompute final exponent
finalExpVal :: Integer
finalExpVal = ((_q ^ _k - 1) `div` _r)

-------------------------------------------------------------------------------
-- Point Lifting
-------------------------------------------------------------------------------

frobex1 :: Num a => Integer -> Point a -> Point a
frobex1 p (Point x y) = Point (x^p) (y^p)
frobex1 p Infinity = Infinity

frobex2 :: Num a => Integer -> Point a -> Point a
frobex2 p (Point x y) = Point (x^(p^2)) (-(y^(p^2)))
frobex2 p Infinity = Infinity

-- frobenius twist 1
twist1 :: Point Fq2 -> Point Fq2
twist1 Infinity = Infinity
twist1 (Point (Fq2 p0 p1) (Fq2 p2 p3)) = Point q0 q1
  where
    q0a = p0
    q0b = -p1
    q0 = Fq2 q0a q0b * xiToPMinus1Over3

    q1a = p2
    q1b = -p3
    q1 = Fq2 q1a q1b * xiToPMinus1Over2

-- frobenius twist 2
twist2 :: Point Fq2 -> Point Fq2
twist2 = twist1 . twist1

toFq12 :: Point Fq -> Point Fq12
toFq12 Infinity = Infinity
toFq12 (Point x y) = Point (Fq12.fq12unit x) (Fq12.fq12unit y)
