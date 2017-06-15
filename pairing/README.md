Pairing Based Cryptography
==============================

Simple implementation of the Optimal Ate Pairing over Barreto-Naehrig Curves.

```
t = 4965661367192848881
q(t) = 36*t^4 + 36*t^3 + 24*t^2 + 6*t + 1
r(t) = 36*t^4 + 36*t^3 + 18*t^2 + 6*t + 1
```

The chosen curve is the `ALT_BN128` curve over a 254-bit prime `q` such that `n
:= q + 1 - t` with embedding degree of `k=12`.

```
E/Fq: y^2 = x^3 + b
```

With:

* `b = 3`
* `q = 21888242871839275222246405745257275088696311157297823662689037894645226208583`

The field `Fq12` is constructed via the following tower:

* `Fq2 = Fq[u] / (u^2 + 1)`
* `Fq6 = Fq2[v] / (v^3 - Xi)` where `Xi = u + 9`
* `Fq12 = Fq6[w] / (w^2 - v)`

The groups used in the pairing:

* The cyclic group `G1` (`Ec1`) is instantiated as `E(Fq)[n]` where `n := q + 1 - t`;
* The cyclic group `G2` (`Ec2`) is instantiated as the inverse image of `E'(Fq^2)[n]` under a twisting isomorphism from `E'` to `E`
* the pairing `e: G1 x G2 -> Fq12` is the optimal ate pairing.
