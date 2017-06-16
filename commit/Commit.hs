{-# LANGUAGE NoImplicitPrelude #-}

module Commit (
  commit,
  open,
  testCommit,
) where

import Protolude

import Crypto.Number.Generate
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types

import qualified Data.ByteArray as BA

secp256k1 :: Curve
secp256k1 = getCurveByName SEC_p256k1

-- secp256k1
q :: Integer
q = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1

g :: Point
g = ecc_g (common_curve secp256k1)

newtype Commitment = Commitment (Integer, Integer, Point)

-- | Commitment
commit :: Integer -> Integer -> IO Commitment
commit a x = do
  -- blinding factor
  r <- generateBetween 0 q
  -- H = aG
  let h = pointBaseMul secp256k1 a
  -- C = xG + rH
  let cm = pointAddTwoMuls secp256k1 x g r h
  pure (Commitment (x, r, cm))

-- | Reveal
open :: Integer -> Commitment -> Bool
open a (Commitment (x, r, cm)) = cm == pointAddTwoMuls secp256k1 x g r h
  where h = pointBaseMul secp256k1 a

testCommit :: IO ()
testCommit = do
  let a = 0xCAFEBEEF -- setup param
  let x = 42         -- secret
  cm <- commit a x
  print $ open a cm          -- should succeed
  print $ open 0xDEADBEEF cm -- should fail
