Pairing Based Cryptography
==============================

Simple implementation of the Optimal Ate Pairing over Barreto-Naehrig Curves.

```
t = 4965661367192848881
p(t) = 36*t^4 + 36*t^3 + 24*t^2 + 6*t + 1
r(t) = 36*t^4 + 36*t^3 + 18*t^2 + 6*t + 1
```

The chosen curve is a BN curve over a 254-bit prime `p` such that `n := p + 1 - t`.

```
E/Fp: y^2 = x^3 + b
```

With:

* `b = 3`
* `p = 21888242871839275222246405745257275088696311157297823662689037894645226208583`

The field `Fp12` is constructed via the following tower:

* `Fp2 = Fp[u] / (u^2 + 1)`
* `Fp6 = Fp2[v] / (v^3 - Xi)` where `Xi = u + 9`
* `Fp12 = Fp6[w] / (w^2 - v)`

The groups used in the pairing:

* The cyclic group `G1` (aka Ec1) is instantiated as `E(Fp)[n]` where `n := p + 1 - t`;
* The cyclic group `G2` (aka Ec2) is instantiated as the inverse image of `E'(Fp^2)[n]` under a twisting isomorphism from `E'` to `E`
* the pairing `e: G1 x G2 -> Fp12` is the optimal ate pairing.
