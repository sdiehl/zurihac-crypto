{-# LANGUAGE Strict #-}

module Fq2 (
  Fq2(..),
  Fq2.new,
  fq2neg,
  fq2add,
  fq2sub,
  fq2mul,
  fq2dbl,
  fq2sqr,
  fq2inv,
  fq2div,
  fq2int,
  fq2one,
  fq2zero,
  Fq2.random,
  Fq2.mulXi,
  Fq2.divXi,
  xi,
  xi_a,
  xi_b,
) where

import Protolude hiding (zero, one)

import Fq
import qualified Params

import Crypto.Random (MonadRandom)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Fq2 = Fq2 { x :: Fq, y :: Fq }
  deriving (Eq, Show)

new :: Fq -> Fq -> Fq2
new = Fq2

instance Num Fq2 where
  (+)         = fq2add
  (*)         = fq2mul
  negate      = fq2neg
  fromInteger = fq2int
  abs         = notImplemented
  signum      = notImplemented

instance Fractional Fq2 where
  (/) = fq2div
  fromRational (a :% b) = fq2int a / fq2int b

xi :: Fq2
xi = Fq2 xi_a xi_b

xi_a, xi_b :: Fq
xi_a = Fq.new Params._xi_a
xi_b = Fq.new Params._xi_b

fq2one :: Fq2
fq2one = fq2int 1

fq2zero :: Fq2
fq2zero = fq2int 0

fq2int :: Integer -> Fq2
fq2int n = Fq2 (fromInteger n) fqZero

fq2neg :: Fq2 -> Fq2
fq2neg (Fq2 x y) = Fq2 (-x) (-y)

fq2add :: Fq2 -> Fq2 -> Fq2
fq2add (Fq2 x y) (Fq2 a b) = Fq2 (x+a) (y+b)

fq2sub :: Fq2 -> Fq2 -> Fq2
fq2sub (Fq2 x y) (Fq2 a b) = Fq2 (x-a) (y-b)

fq2dbl :: Fq2 -> Fq2
fq2dbl (Fq2 x y) = Fq2 (shiftL x 1) (shiftL y 1)

fq2div :: Fq2 -> Fq2 -> Fq2
fq2div a b = fq2mul a (fq2inv b)

fq2mul :: Fq2 -> Fq2 -> Fq2
fq2mul (Fq2 a0 a1) (Fq2 b0 b1) = Fq2 c0 c1
  where
    aa = a0 * b0
    bb = a1 * b1
    c0 = bb * fqNqr + aa
    c1 = (a0 + a1) * (b0 + b1) - aa - bb

mulXi :: Fq2 -> Fq2
mulXi = (* xi)

divXi :: Fq2 -> Fq2
divXi = (/ xi)

fq2sqr :: Fq2 -> Fq2
fq2sqr (Fq2 a0 a1) = Fq2 c0 c1
  where
    aa = a0 * a0
    bb = a1 * a1
    c0 = bb * fqNqr + aa
    c1 = (a0 + a1) * (a0 + a1) - aa - bb

fq2iszero :: Fq2 -> Bool
fq2iszero = (== fq2zero)

fq2inv :: Fq2 -> Fq2
fq2inv (Fq2 a0 a1) = Fq2 c0 c1
  where
    t = fqInv ((a0 ^ 2) - ((a1 ^ 2) * fqNqr))
    c0 = a0 * t
    c1 = -(a1 * t)

random :: MonadRandom m => m Fq2
random = do
  x <- Fq.random
  y <- Fq.random
  pure (Fq2 x y)
