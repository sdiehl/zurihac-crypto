{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Fq12 (
  Fq12(..),
  fq12,
  fq12int,
  fq12unit,
  fq12inv,
  fq12one,
  fq12zero,
  fq12add,
  fq12sub,
  fq12neg,
  fq12pow,
  fq12mul,
  random,
) where

import Protolude

import Fq (Fq)
import Fq2 (Fq2(..))
import Fq6 (Fq6(..))

import qualified Fq
import qualified Fq2
import qualified Fq6

import Crypto.Random (MonadRandom)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data Fq12 = Fq12 { x :: Fq6, y :: Fq6 }
  deriving (Eq, Show)

instance Num Fq12 where
  (+)         = fq12add
  (*)         = fq12mul
  negate      = fq12neg
  fromInteger = fq12int
  abs         = notImplemented
  signum      = notImplemented

instance Fractional Fq12 where
  (/) = fq12div
  fromRational (a :% b) = fq12int a / fq12int b

random :: MonadRandom m => m Fq12
random = do
  x <- Fq6.random
  y <- Fq6.random
  pure (Fq12 x y)

fq12 :: [Fq] -> Fq12
fq12 [a,b,c,d,e,f,g,h,i,j,k,l] = Fq12
  { x = Fq6.new (Fq2.new a b) (Fq2.new c d) (Fq2.new e f)
  , y = Fq6.new (Fq2.new g h) (Fq2.new i j) (Fq2.new k l)
  }
fq12 _ = panic "Invalid arguments to fq12"

fq12int :: Integer -> Fq12
fq12int n = fq12 (fromIntegral n : replicate 11 0)

fq12unit :: Fq -> Fq12
fq12unit n = fq12 (n : replicate 11 0)

fq12one :: Fq12
fq12one = fq12int 1

fq12zero :: Fq12
fq12zero = fq12int 0

fq12add :: Fq12 -> Fq12 -> Fq12
fq12add (Fq12 x y) (Fq12 a b) = Fq12 (x+a) (y+b)

fq12pow :: Fq12 -> Integer -> Fq12
fq12pow x 0 = fq12one
fq12pow x 1 = x
fq12pow x n
  | even n = fq12pow (x * x) (n `div` 2)
  | odd n = (fq12pow (x * x) (n `div` 2)) * x

fq12sub :: Fq12 -> Fq12 -> Fq12
fq12sub (Fq12 x y) (Fq12 a b) = Fq12 (x-a) (y-b)

fq12neg :: Fq12 -> Fq12
fq12neg (Fq12 x y) = Fq12 (negate x) (negate y)

fq12div :: Fq12 -> Fq12 -> Fq12
fq12div a b = a * fq12inv b

fq12mul :: Fq12 -> Fq12 -> Fq12
fq12mul (Fq12 x y) (Fq12 a b) = Fq12 (Fq6.mulXi bb + aa) ((x+y) * (a+b) - aa - bb)
  where
    aa = x*a
    bb = y*b

fq12inv :: Fq12 -> Fq12
fq12inv (Fq12 a b) = Fq12 (a*t) (-(b*t))
  where
    t = Fq6.fq6inv (a^2 - Fq6.mulXi (b^2))
