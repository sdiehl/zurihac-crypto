{-# LANGUAGE NoImplicitPrelude #-}

module Group (
  g1,
  g2,
  b1,
  b2,

  xiToPMinus1Over2,
  xiToPMinus1Over3,
  xiToPMinus1Over6,

  w,
  w2,
  w3,
  twist,
  twist_mulX,
  twist_mulY,
) where

import Protolude

import Fq
import Fq2
import Fq12
import Point
import Params

-- | Generator for G1
g1 :: Point Fq
g1 = Point 1 2

-- | Generator for G2
g2 :: Point Fq2
g2 = Point x y
  where
    x = Fq2
      10857046999023057135944570762232829481370756359578518086990519993285655852781
      11559732032986387107991004021392285783925812861821192530917403151452391805634

    y = Fq2
      8495653923123431417604973247489272438418190587263600148770280649306958101930
      4082367875863433681332203403145435568316851327593401208105741076214120093531

b1 :: Fq
b1 = 3

-- Twisted curve over FQ^2
b2 :: Fq2
b2 = (Fq2 3 0) / Fq2.xi

-- Parameters for twisted short Weierstrass curve
-- E'/Fq2 : y^2 = x^3 + (a * twist^2) * x + (b * twist^3)
twist :: Fq2
twist = Fq2.xi

twistNqr :: Fq2
twistNqr = Fq2 2 0

twist_mulX :: Fq2
twist_mulX = Fq2
  21575463638280843010398324269430826099269044274347216827212613867836435027261
  10307601595873709700152284273816112264069230130616436755625194854815875713954

twist_mulY :: Fq2
twist_mulY = Fq2
  2821565182194536844548159561693502659359617185244120367078079554186484126554
  3505843767911556378687030309984248845540243509899259641013678093033130930403

-- omega point
w, w2, w3 :: Fq12
w = Fq12.fq12 ([0,1] ++ replicate 10 0)
w2 = w^2
w3 = w^3

-- ξ^((p-1)/6)
xiToPMinus1Over6 :: Fq2
xiToPMinus1Over6 = xi ^ ((_q-1) `div` 6)

-- ξ^((p-1)/3)
xiToPMinus1Over3 :: Fq2
xiToPMinus1Over3 = xi ^ ((_q-1) `div` 3)

-- ξ^((p-1)/2)
xiToPMinus1Over2 :: Fq2
xiToPMinus1Over2 = xi ^ ((_q-1) `div` 2)

-- ξ^((2p²-2)/3)
-- a cubic root of unity, mod p
xiTo2PSquaredMinus2Over3 :: Fq2
xiTo2PSquaredMinus2Over3 = xi ^ ((2*_q^2-2) `div` 3)

-- ξ^((2p²-2)/6)
-- a cubic root of -1, mod p
xiTo2PSquaredMinus2Over6 :: Fq2
xiTo2PSquaredMinus2Over6 = xi ^ ((2*_q^2-2) `div` 6)
