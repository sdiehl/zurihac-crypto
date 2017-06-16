{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fq (
  Fq(..),

  new,
  fqAdd,
  fqSub,
  fqSqr,
  fqDbl,
  fqDiv,
  fqMul,
  fqInv,
  fqNeg,

  fqZero,
  fqOne,
  fqNqr,

  random,
) where

import Protolude

import Params

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

newtype Fq = Fq Integer
  deriving (Show, Eq, Bits)

instance Num Fq where
  (+)           = fqAdd
  (*)           = fqMul
  abs           = fqAbs
  signum        = fqSig
  negate        = fqNeg
  fromInteger n = Fq (n `mod` _q)

instance Fractional Fq where
  (/) = fqDiv
  fromRational (a :% b) = Fq a / Fq b

new :: Integer -> Fq
new a = Fq (a `mod` _q)

{-# INLINE norm #-}
norm :: Fq -> Fq
norm (Fq a) = Fq (a `mod` _q)

{-# INLINE fqAdd #-}
fqAdd :: Fq -> Fq -> Fq
fqAdd (Fq a) (Fq b) = norm (Fq (a+b))

{-# INLINE fqSub #-}
fqSub :: Fq -> Fq -> Fq
fqSub (Fq a) (Fq b) = norm (Fq (a-b))

{-# INLINE fqMul #-}
fqMul :: Fq -> Fq -> Fq
fqMul (Fq a) (Fq b) = norm (Fq (a*b))

{-# INLINE fqAbs #-}
fqAbs :: Fq -> Fq
fqAbs (Fq a) = (Fq a)

{-# INLINE fqDbl #-}
fqDbl :: Fq -> Fq
fqDbl (Fq a) = Fq (shiftL a 1 `mod` _q)

{-# INLINE fqSqr #-}
fqSqr :: Fq -> Fq
fqSqr a = fqMul a a

fqSig :: Fq -> Fq
fqSig (Fq a) = Fq (signum a  `mod` _q)

{-# INLINE fqNeg #-}
fqNeg :: Fq -> Fq
fqNeg (Fq a) = Fq ((-a) `mod` _q)

{-# INLINE fqDiv #-}
fqDiv :: Fq -> Fq -> Fq
fqDiv a b = fqMul a (inv b)

{-# INLINE fqNqr #-}
fqNqr :: Fq
fqNqr = Fq Params._nqr

{-# INLINE fqInv #-}
fqInv :: Fq -> Fq
fqInv x = 1 / x

{-# INLINE fqZero #-}
fqZero :: Fq
fqZero = Fq 0

{-# INLINE fqOne #-}
fqOne :: Fq
fqOne = Fq 1

random :: MonadRandom m => m Fq
random = do
  seed <- generateMax _q
  pure (Fq seed)

inv :: Fq -> Fq
inv (Fq a) = Fq ((euclidean a _q) `mod` _q)

euclidean :: (Integral a) => a -> a -> a
euclidean a b = fst (inv' a b)

{-# INLINEABLE inv' #-}
{-# SPECIALISE inv' :: Integer -> Integer -> (Integer, Integer) #-}
inv' :: (Integral a) => a -> a -> (a, a)
inv' a b =
  case b of
   1 -> (0, 1)
   _ -> let (e, f) = inv' b d
        in (f, e - c*f)
  where c = a `div` b
        d = a `mod` b
