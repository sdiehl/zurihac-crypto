{-# LANGUAGE Strict #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Fr (
  Fr(..),
  new,
  random,
) where

import Protolude

import Params

import Crypto.Random (MonadRandom)
import Crypto.Number.Generate (generateMax)

instance Num Fr where
  (+)           = frAdd
  (*)           = frMul
  abs           = frAbs
  signum        = frSig
  negate        = frNeg
  fromInteger n = Fr (n `mod` _r)

newtype Fr = Fr Integer
  deriving (Show, Eq, Bits)

new :: Integer -> Fr
new a = Fr (a `mod` _r)

{-# INLINE norm #-}
norm :: Fr -> Fr
norm (Fr a) = Fr (a `mod` _r)

{-# INLINE frAdd #-}
frAdd :: Fr -> Fr -> Fr
frAdd (Fr a) (Fr b) = norm (Fr (a+b))

{-# INLINE frSub #-}
frSub :: Fr -> Fr -> Fr
frSub (Fr a) (Fr b) = norm (Fr (a-b))

{-# INLINE frMul #-}
frMul :: Fr -> Fr -> Fr
frMul (Fr a) (Fr b) = norm (Fr (a*b))

{-# INLINE frAbs #-}
frAbs :: Fr -> Fr
frAbs (Fr a) = (Fr a)

{-# INLINE frDbl #-}
frDbl :: Fr -> Fr
frDbl (Fr a) = Fr (shiftL a 1 `mod` _r)

{-# INLINE frSqr #-}
frSqr :: Fr -> Fr
frSqr a = frMul a a

{-# INLINE frSig #-}
frSig :: Fr -> Fr
frSig (Fr a) = Fr (signum a  `mod` _r)

{-# INLINE frNeg #-}
frNeg :: Fr -> Fr
frNeg (Fr a) = Fr ((-a) `mod` _r)

random :: MonadRandom m => m Fr
random = do
  seed <- generateMax _r
  pure (Fr seed)
