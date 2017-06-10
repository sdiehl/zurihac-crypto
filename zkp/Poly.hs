{-# LANGUAGE NoImplicitPrelude #-}

module Poly (
  interp,
) where

import Protolude

interp :: (Eq b, Fractional b) => [(b, b)] -> b -> b
interp xys x = sum $ zipWith (*) ys $ map f xs 
  where
    xs = map fst xys
    ys = map snd xys
    f xj = product $ map (p xj) xs
    p xj xm = if xj == xm then 1 else (x - xm) / (xj - xm)
