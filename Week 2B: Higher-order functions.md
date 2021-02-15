### Higher-order functions 
Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.

- The function map :: (a -> b) -> [a] -> [b] applying the given function to each element of the list.
- The function filter :: (a -> Bool) -> [a] -> [a] removing all elements from a list that do not satisfy the given predicate.
- The function all :: (a -> Bool) -> [a] -> Bool deciding if all elements of a list satisfy the given predicate.
- The function any :: (a -> Bool) -> [a] -> Bool deciding if any element of a list satisfies the given predicate.
- The function takeWhile :: (a -> Bool) -> [a] -> [a] selecting all elements from a list until the first element that does not satisfy the given predicate.
- The function dropWhile :: (a -> Bool) -> [a] -> [a] removing all elements from a list until the first element that does not satisfy the given predicate.

Use each of the following techniques at least once:

- Using a list comprehension
- Using explicit recursion
- Using the library function foldr

```haskell
map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]
-- map f = foldr (\x xs -> f x : xs) []

filter :: (a -> Bool) -> [a] -> [a]
filter p xs = [ x | x <- xs, (p x)]
-- filter p = foldr (\x xs -> if p x then x : xs else xs) []

all :: (a -> Bool) -> [a] -> Bool
all p [] = True
all p [x] = (p x)
all p (x:xs) = (p x) && (all p xs)
-- all p = and . map p

any :: (a -> Bool) -> [a] -> Bool
any p []  = False
any p [x] = (p x)
any p (x:xs) = (p x) || (any p xs)
-- any p = or . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else acc) []
-- takeWhile p [] = []
-- takeWhile p (x:xs)
--     | p x       = x : takeWhile p xs
--     | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p xs = foldr (\a r b -> if b && p a then r True else a:r False) (const []) xs True
-- dropWhile p [] = []
-- dropWhile p (x:xs)
--     | p x       = dropWhile p xs
--     | otherwise = x:xs
```
__________________________________________________________________________________________________________________________________________________________________
### Lemon curry
Currying (named after [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry)) is the process of turning a function taking a pair as its argument into a function that takes two separate arguments. Conversely, uncurrying 

For this exercise, reimplement the two standard Haskell functions

```haskell
curry :: ((a, b) -> c) -> (a -> b -> c)
uncurry :: (a -> b -> c) -> ((a, b) -> c)
```

```haskell
import Prelude hiding (curry, uncurry)
import qualified Test.QuickCheck.Function as Test

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f = \x y -> f (x, y) -- applies f to the tuple (x,y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f = \(x,y) -> f x y -- applies f to x afterwards to y
```
__________________________________________________________________________________________________________________________________________________________________
### Decimal decoding 
Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number (encoded as a list of digits) into an integer. For example:
```haskell
> dec2int [2,3,4,5]
2345
```

```haskell
dec2int :: [Int] -> Int
-- dec2int = foldl (\x  y -> 10*x + y) 0 
dec2int xs = foldl (\x  y -> 10*x + y) 0 xs
```
__________________________________________________________________________________________________________________________________________________________________
### Unfold

A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:
```haskell
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
```
That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function int2bin converting an integer to a binary number can be defined using unfold as follows:
```haskell
int2bin = unfold (== 0) (`mod 2`) (`div` 2)
```
Redefine the functions map :: (a -> b) -> [a] -> [b] and iterate :: (a -> a) -> a -> [a] in terms of unfold.

```haskell
import Prelude hiding (map, iterate)
import qualified Prelude

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)
               
map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) tail

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (const False) id f               
````
__________________________________________________________________________________________________________________________________________________________________
### Reindexing

Implement a function reindex :: (Int -> Int) -> [a] -> [a] that rearranges the elements of the given list according to the given function: the element of reindex f xs at position f i should be the same as the element of xs at position i. In other words, the result should satisfy the equation reindex f xs !! (f i) == xs !! i. For example:
```haskell
> reindex (\i -> 4-i) [1,2,3,4,5]
[5,4,3,2,1]

> reindex (\i -> (i+2) `mod` 5) [1,2,3,4,5]
[4,5,1,2,3]
```

```haskell
traverser f i c xs = if ((f c) == i) then (xs !! c) else traverser f i (c+1) xs 
helper i f xs 
  | i == length xs = []
  | otherwise = (traverser f i 0 xs):(helper (i+1) f xs) 
reindex f xs = helper 0 f xs

-- reindex :: (Int -> Int) -> [a] -> [a]
-- reindex f xs = [xs !! i | i <- indices]
--   where
--     indices = map f [0..(length xs-1)]

-- Alternatively:

import Data.List
import Data.Function

reindex :: (Int -> Int) -> [a] -> [a]
reindex f xs = map snd (sortBy (compare `on` fst) newList) 
  where
    indices = map f [0..]
    xss (x,_) = x >= 0 && x < (length xs)
    newList = filter xss (zip indices xs)
 ```
