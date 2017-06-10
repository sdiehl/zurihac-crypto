{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Paillier (
  genKeys,
  encrypt,
  decrypt,
  homoAdd,
  homoMul,
) where

import Protolude

import Crypto.Number.Prime (generatePrime)
import Crypto.Number.Generate (generateMax)
import Crypto.Number.ModArithmetic (expSafe, inverse)


data PublicKey = PublicKey { n :: Integer, g :: Integer } 
  deriving Show 

data PrivateKey = PrivateKey { l :: Integer, u :: Integer } 
  deriving Show 

newtype PlainText = PlainText Integer 
  deriving (Show)

newtype CipherText = CipherText Integer 
  deriving (Show, Num)

carmichael :: Integer -> Integer -> Integer
carmichael p q = lcm (p-1) (q-1)

genKeys :: Int -> IO (PublicKey, PrivateKey)
genKeys bits = do
  p <- generatePrime bits
  q <- generatePrime bits
  let n = p*q
  let n2 = n*n
  let l = carmichael p q
  g <- liftIO $ generateMax (n2 -1) >>= createG n2
  let pubKey = PublicKey { n = n, g = g }
  case inverse (mod1n pubKey (expSafe g l (n*n))) n of
    Just u -> return (pubKey, PrivateKey { l = l, u = u })
    Nothing -> panic "Improbably unlucky numbers."

encrypt :: PlainText -> PublicKey -> IO CipherText
encrypt (PlainText m) pk@PublicKey{..} = do
  let n2 = n*n
  let g_m = expSafe g m n2
  r <- generateMax (n-1) >>= createR pk
  let r_n = expSafe r n n2
  return $ CipherText $ expSafe (g_m * r_n) 1 n2

createR :: PublicKey -> Integer -> IO Integer
createR pk@PublicKey{..} r
  | gcd r n == 1 = return r
  | otherwise = generateMax (n-1) >>= createR pk

createG :: Integer -> Integer -> IO Integer
createG n2 g
  | gcd g n2 == 1 = return g
  | otherwise = generateMax (n2-1) >>= createG n2

decrypt :: CipherText -> PrivateKey -> PublicKey -> PlainText
decrypt (CipherText c) PrivateKey{..} pub@PublicKey{..} =
  PlainText $ expSafe (mod1n pub (expSafe c l (n*n)) * u) 1 n

mod1n :: PublicKey -> Integer -> Integer
mod1n PublicKey{..} x = (x-1) `div` n

-- E(a + b) given E(a) and E(b).
homoAdd :: PublicKey -> CipherText -> CipherText -> CipherText
homoAdd PublicKey{..} (CipherText x) (CipherText y) = CipherText ((x * y) `mod` (n*n*n*g))

-- E(a)*b given E(a) and b
homoMul :: PublicKey -> CipherText -> Integer -> CipherText
homoMul PublicKey {..} (CipherText c1) p1 = CipherText (expSafe c1 p1 (n*n*n*g))

example :: IO ()
example = do
  (pk, sk) <- genKeys 256
  a <- encrypt (PlainText 42) pk
  b <- encrypt (PlainText 43) pk

  let t1 = homoAdd pk a b
  let t2 = homoMul pk a 2

  print $ decrypt t1 sk pk
  print $ decrypt t2 sk pk
