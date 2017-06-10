{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fq (
  Fq(..),

  new,
  norm,

  fpAdd,
  fpSub,
  fpSqr,
  fpDbl,
  fpDiv,
  fpMul,
  fpInv,

  fpZero,
  fpOne,
  fpNqr,

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
  (+)           = fpAdd
  (*)           = fpMul
  abs           = fpAbs
  signum        = fpSig
  negate        = fpNeg
  fromInteger n = Fq (n `mod` _p)

instance Fractional Fq where
  (/) = fpDiv
  fromRational (a :% b) = Fq a / Fq b

new :: Integer -> Fq
new a = Fq (a `mod` _p)

{-# INLINE norm #-}
norm :: Fq -> Fq
norm (Fq a) = Fq (a `mod` _p)

{-# INLINE fpAdd #-}
fpAdd :: Fq -> Fq -> Fq
fpAdd (Fq a) (Fq b) = norm (Fq (a+b))

{-# INLINE fpSub #-}
fpSub :: Fq -> Fq -> Fq
fpSub (Fq a) (Fq b) = norm (Fq (a-b))

{-# INLINE fpMul #-}
fpMul :: Fq -> Fq -> Fq
fpMul (Fq a) (Fq b) = norm (Fq (a*b))

{-# INLINE fpAbs #-}
fpAbs :: Fq -> Fq
fpAbs (Fq a) = (Fq a)

{-# INLINE fpDbl #-}
fpDbl :: Fq -> Fq
fpDbl (Fq a) = Fq (shiftL a 1 `mod` _p)

{-# INLINE fpSqr #-}
fpSqr :: Fq -> Fq
fpSqr a = fpMul a a

fpSig :: Fq -> Fq
fpSig (Fq a) = Fq (signum a  `mod` _p)

{-# INLINE fpNeg #-}
fpNeg :: Fq -> Fq
fpNeg (Fq a) = Fq ((-a) `mod` _p)

{-# INLINE fpDiv #-}
fpDiv :: Fq -> Fq -> Fq
fpDiv a b = fpMul a (inv b)

{-# INLINE fpNqr #-}
fpNqr :: Fq
fpNqr = Fq Params._nqr

{-# INLINE fpInv #-}
fpInv :: Fq -> Fq
fpInv x = 1 / x

{-# INLINE fpZero #-}
fpZero :: Fq
fpZero = Fq 0

{-# INLINE fpOne #-}
fpOne :: Fq
fpOne = Fq 1

random :: MonadRandom m => m Fq
random = do
  seed <- generateMax _p
  pure (Fq seed)

inv :: Fq -> Fq
inv (Fq a) = Fq ((euclidean a _p) `mod` _p)

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
