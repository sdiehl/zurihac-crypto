Elliptic Curve Commitment Schemes
================================

a commitment scheme allows one to commit to a chosen value (or chosen statement)
while keeping it hidden to others, with the ability to reveal the committed
value later. A commitment scheme allows one to commit to a chosen value (or
chosen statement) while keeping it hidden to others, with the ability to
reveal the committed value later.

Basic example of the Pedersen commitment scheme using elliptic curves to create
a binding and computationally hiding scheme.

```haskell
testCommit :: IO ()
testCommit = do
  let a = 0xCAFEBEEF -- setup param
  let x = 42         -- secret
  cm <- commit a x
  print $ open a cm          -- should succeed
  print $ open 0xDEADBEEF cm -- should fail
```
