module Params (
  _a,
  _b,
  _p,
  _r,
  _t,
  _k,
  _l,
  _trace,
  _nqr,
  _inv,
  _xi_a,
  _xi_b,
  _nbits,
  curve_order,
) where

import Protolude
import Crypto.Number.Basic (numBits)

{-

The curve equation for a BN curve is:
E/Fp: y^2 = x^3 + b

Referenecs:

1. [BCGTV13] https://eprint.iacr.org/2013/507
2. [BCTV14] https://eprint.iacr.org/2013/879

-}

-- Curve coefficent
_b  :: Integer
_b = 3

-- Curve coefficent
_a  :: Integer
_a = 0

-- Embedding degree
_k  :: Integer
_k = 12

-- BN parameter that determines the prime
_t :: Integer
_t = 4965661367192848881

-- Fq
_p :: Integer
_p = 36*_t^4 + 36*_t^3 + 24*_t^2 + 6*_t + 1

-- Fr
_r :: Integer
_r = 36*_t^4 + 36*_t^3 + 18*_t^2 + 6*_t + 1

_l :: Integer
_l = (_p - 1) `div` 6

-- Froebenius trace
_trace :: Integer
_trace = 6*_t^2 + 2

-- xi = xi_a + i
_xi_a :: Integer
_xi_a = 9

_xi_b :: Integer
_xi_b = 1

-- Number of bits
_nbits :: Int
_nbits = numBits _p

_inv :: Integer
_inv = 0xe4866389

-- quadratic nonresidue
_nqr :: Integer
_nqr = 21888242871839275222246405745257275088696311157297823662689037894645226208582

_rsquared :: Integer
_rsquared = 3096616502983703923843567936837374451735540968419076528771170197431451843209

_rcubed :: Integer
_rcubed = 14921786541159648185948152738563080959093619838510245177710943249661917737183

curve_order :: Integer
curve_order = 21888242871839275222246405745257275088548364400416034343698204186575808495617
