{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Gates where

import Protolude

type Name = Text

-- Language just expressive enough to implement a hash function.
data Term a where
  Lit    :: Bits a => a -> Term a
  And    :: Bits a => Term a -> Term a -> Term a
  Not    :: Bits a => Term a -> Term a
  Nand   :: Bits a => Term a -> Term a -> Term a
  Or     :: Bits a => Term a -> Term a -> Term a
  Xor    :: Bits a => Term a -> Term a -> Term a

  Add    :: Term Int -> Term Int -> Term Int
  Mul    :: Term Int -> Term Int -> Term Int

  Rotate :: Bits a => Term a -> Term Int -> Term a
  SetBit :: Bits a => Term a -> Term Int -> Term a

  If     :: Term Bool -> Term a -> Term a -> Term a
  Arg    :: Name -> Term Name

data Gate
  = GAdd Gate Gate
  | GMul Gate Gate
  | GNeg Gate
  | GOne
  | GZero
  | GArith [Word]
  | Input Name
  | Output Name Gate
  deriving (Eq, Ord, Show)

eval :: Bits a => Term a -> a
eval (Lit i)      = i
eval (Arg i)      = i
eval (Add a b)    = eval a + eval b
eval (Mul a b)    = eval a * eval b
eval (Xor a b)    = eval a `xor` eval b
eval (And a b)    = eval a .&. eval b
eval (Or a b)     = eval a .|. eval b
eval (Not a)      = complement (eval a)
eval (Nand a b)   = eval (Not (And a b))
eval (Rotate a b) = eval a `rotate` eval b
eval (SetBit a b) = eval a `setBit` eval b
eval (If b e1 e2) = if eval b then eval e1 else eval e2 

compile :: ToBytes a => Term a -> Gate
compile (Lit a) = bytes a
compile (Arg a) = Input a
compile (And a b) = (compile a) * (compile b)
compile (Not a) = GOne - compile a
compile (Nand a b) = 1 - compile a * compile b
compile (Or a b) = 1 - (1 - compile a) * (1 - compile b)
compile (Xor a b) = (compile a + compile b) - ((compile a) * (compile b) + (compile a) * (compile b))
compile (If b e1 e2) = (compile e1 * compile b) + (compile e2 * compile b)
compile _ = notImplemented

class FiniteBits a => ToBytes a where
  bytes :: a -> Gate

instance ToBytes Int8 where
  bytes = GArith . bytesI

instance ToBytes Bool where
  bytes True = GOne
  bytes False = GZero

bytesI :: (FiniteBits b) => b -> [Word]
bytesI b = fmap (bool 0 1 . testBit b) [0 .. finiteBitSize b - 1]

instance Num Gate where
  negate = GNeg
  (+) = GAdd
  (*) = GMul
  abs = notImplemented
  signum = notImplemented
  fromInteger = notImplemented

sample :: Gate
sample = Output "c6" (GMul (GAdd c1 c2) (GMul c3 c4))
  where
    c1 = Input "c1"
    c2 = Input "c2"
    c3 = Input "c3"
    c4 = Input "c4"
    c5 = Output "c5"

lefts' :: Gate -> [Int]
lefts' = \case
  GAdd a b    -> [0] ++ lefts' a ++ lefts' b
  GMul a b    -> [1] ++ lefts' a ++ lefts' b
  GNeg a      -> lefts' a
  GOne        -> [0]
  GZero       -> [0]
  GArith _    -> [0]
  Input _     -> [0]
  Output nm g -> lefts' g

outputs' :: Int -> Gate -> [(Int, Name)]
outputs' i = \case
  GAdd a b    -> outputs' (i+1) a ++ outputs' (i+2) b
  GMul a b    -> outputs' (i+1) a ++ outputs' (i+2) b
  GNeg a      -> outputs' (i+1) a
  GOne        -> []
  GZero       -> []
  GArith _    -> []
  Input _     -> []
  Output nm g -> [(i, nm)]

outputs :: Gate -> [(Int, Name)]
outputs = outputs' 0
