{-# LANGUAGE NoImplicitPrelude #-}

module Sigma (
  proof,
  verifyProof,
  testProof,
) where

import Protolude

import Crypto.Hash
import Crypto.Number.F2m
import Crypto.Random.Types
import Crypto.Number.Generate
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Crypto.Number.Serialize

import qualified Data.ByteArray as BA

newtype Proof = Proof (Point, Integer)
  deriving (Eq, Show)

secp256k1 :: Curve
secp256k1 = getCurveByName SEC_p256k1

q :: Integer
q = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1

p :: Integer
p = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEBAAEDCE6AF48A03BBFD25E8CD0364141

g :: Point
g = ecc_g (common_curve secp256k1)

oracle :: ByteString -> Integer
oracle x = os2ip (sha256 x) `mod` p

sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hash bs :: Digest SHA3_256)

-- ZKPok{ (a): A = g^a }
proof :: MonadRandom m => Integer -> m Proof
proof a = do
  -- blinding factor
  k <- generateBetween 0 p
  -- commmitment
  let k' = pointBaseMul secp256k1 k
  let c = oracle (show k')
  let s = (k + c*a) `mod` p
  pure (Proof (k', s))

verifyProof :: Point -> Proof -> Bool
verifyProof a' (Proof (k',s)) = lhs == rhs
  where
    lhs = pointBaseMul secp256k1 s
    rhs = pointAdd secp256k1 k' (pointMul secp256k1 c a')
    c   = oracle (show k')

testProof :: IO Bool
testProof = do
  let a = 42
  let a' = pointBaseMul secp256k1 a 
  prf <- proof a
  print (a', prf)
  pure (verifyProof a' prf)
