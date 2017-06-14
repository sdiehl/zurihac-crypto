{-# LANGUAGE Strict #-}

module Fq6 (
  Fq6(..),
  new,
  fq6add,
  fq6sub,
  fq6mul,
  fq6dbl,
  fq6inv,
  fq6sqr,
  fq6div,
  fq6int,
  fq6neg,
  fq6one,
  fq6zero,
  mulXi,
  random,
) where

import Protolude hiding (zero, one)

import Fq (Fq)
import Fq2 (Fq2)

import qualified Fq2

import Crypto.Random (MonadRandom)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Fq6 = Fq6 { x :: Fq2, y :: Fq2, z :: Fq2 }
  deriving (Eq, Show)

instance Num Fq6 where
  (+)         = fq6add
  (*)         = fq6mul
  negate      = fq6neg
  fromInteger = fq6int
  abs         = notImplemented
  signum      = notImplemented

new :: Fq2 -> Fq2 -> Fq2 -> Fq6
new = Fq6

fq6zero :: Fq6
fq6zero = Fq6 0 0 0

fq6int :: Integer -> Fq6
fq6int n = Fq6 (fromInteger n) 0 0

fq6one :: Fq6
fq6one = Fq6 1 0 0

fq6add :: Fq6 -> Fq6 -> Fq6
fq6add (Fq6 x y z) (Fq6 a b c) = Fq6 (x+a) (y+b) (z+c)

fq6sub :: Fq6 -> Fq6 -> Fq6
fq6sub (Fq6 x y z) (Fq6 a b c) = Fq6 (x-a) (y-b) (z-c)

fq6div :: Fq6 -> Fq6 -> Fq6
fq6div a b = a * fq6inv b

fq6neg :: Fq6 -> Fq6
fq6neg (Fq6 x y z) = Fq6 (-x) (-y) (-z)

fq6dbl :: Fq6 -> Fq6
fq6dbl (Fq6 x y z) = Fq6 (Fq2.fq2dbl x) (Fq2.fq2dbl y) (Fq2.fq2dbl z)

-- XXX: more optimal implementation
fq6sqr :: Fq6 -> Fq6
fq6sqr x = x^2

fq6mul :: Fq6 -> Fq6 -> Fq6
fq6mul (Fq6 a0 a1 a2) (Fq6 b0 b1 b2) = Fq6 c0 c1 c2
  where
    t0 = a0 * b0
    t1 = a1 * b1
    t2 = a2 * b2
    c0 = Fq2.mulXi ((a1+a2) * (b1+b2) - t1 - t2) + t0
    c1 = ((a0+a1) * (b0+b1)) - t0 - t1 + (Fq2.mulXi t2)
    c2 = ((a0+a2) * (b0+b2)) - t0 + t1 - t2

mulXi :: Fq6 -> Fq6
mulXi (Fq6 x y z) = Fq6 (Fq2.mulXi z) x y

divXi :: Fq6 -> Fq6
divXi (Fq6 x y z) = Fq6 (z/Fq2.xi) x y

fq6inv :: Fq6 -> Fq6
fq6inv (Fq6 a b c) = Fq6 (t*c0) (t*c1) (t*c2)
  where
    c0 = a^2 - b * c * Fq2.xi
    c1 = c^2 * Fq2.xi - a * b
    c2 = b^2 - a*c
    t  = Fq2.fq2inv ((c * c1 + b * c2) * Fq2.xi + a*c0)

random :: MonadRandom m => m Fq6
random = do
  a <- Fq2.random
  b <- Fq2.random
  c <- Fq2.random
  pure (Fq6 a b c)
