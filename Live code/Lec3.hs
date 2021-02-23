module Lec3 where

import Data.Char (isLower, toUpper)

applys :: [a -> b] -> [a] -> [b]
applys fs xs = map app (zip fs xs)
  where
      app (f,x) = f x

applys' :: [a -> b] -> [a] -> [b]
applys' (f:fs) (x:xs) = f x : applys fs xs
applys' _      _      = []

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \x y -> f y x


first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

second :: (b -> c) -> (a,b) -> (a,c)
second f (x,y) = (x, f y)

(***) :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
(f *** g) (x,y) = (f x, g y)

allCaps :: String -> String
allCaps xs = [toUpper x | x <- xs]

allCaps' :: String -> String
allCaps' ""     = ""
allCaps' (x:xs) = toUpper x : allCaps xs

allCaps'' :: String -> String
allCaps'' = map toUpper
