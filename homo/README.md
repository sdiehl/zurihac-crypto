Partial Homomorphic Encryption
==============================

The Paillier is a malleable partially homomorphic system with the ability to
perform limited arithmetic operations on cryptotext.

```haskell
example :: IO ()
example = do
  (pk, sk) <- genKeys 256
  a <- encrypt (PlainText 42) pk
  b <- encrypt (PlainText 43) pk
  let t1 = homoAdd a b
  let t2 = homoMul pk a 2
  print $ decrypt t1 sk pk
  print $ decrypt t2 sk pk
```
