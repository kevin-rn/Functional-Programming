### Fibonacci
Using a list comprehension, define an expression fibs :: [Integer] that generates the infinite list of Fibonacci numbers
0,1,1,2,3,5,8,13,21,34,⋯

using the following simple procedure:
- the first two numbers are 0 and 1;
- the next is the sum of the previous two;
- return to the second step.

Hint: make use of the library functions zip and tail. Note that numbers in the Fibonacci sequence quickly become large, hence the use of the type Integer of arbitrary-precision integers above.  

##### Solution:
```haskell
fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]
```

________________________________________________________________________________________________________________________________________________________

### Newton's method
Newton’s method for computing the square root of a (non-negative) floating-point number n can be expressed as follows:
- start with an initial approximation to the result;
- given the current approximation a, the next approximation is defined by the function next a = (a + n/a) / 2
- repeat the second step until the two most recent approximations are within some desired distance of one another, at which point the most recent value is returned as the result.

Define a function sqroot :: Double -> Double that implements this procedure.

Hint: first produce an infinite list of approximations using the library function iterate. For simplicity, take the number 1.0 as the initial approximation, and 0.00001 as the distance value.

##### Solution:
```haskell
next :: Double -> Double -> Double
next n a = (a + (n / a) ) / 2

searchapprox :: Double -> [Double] -> Double
searchapprox n [x] = x
searchapprox n (x:xs) = if abs(x*x - n) < 0.00000001 then x
                                else searchapprox n xs

sqroot :: Double -> Double
sqroot  n = searchapprox n approximations
  where 
    approximations = iterate (next n) 1.0
            


```

________________________________________________________________________________________________________________________________________________________

### Prime numbers
Write a function primes :: [Integer] that returns the infinite list of all prime numbers.

Hint. First implement a function sieve :: [Integer] -> [Integer] that uses the Sieve of Eratosthenes to filter out any elements that are a multiple of a previous element, and then apply this function to the infinite list [2..].

##### Solution:
```haskell

-- sieve :: [Integer] -> [Integer]
-- sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

primes :: [Integer]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

```

________________________________________________________________________________________________________________________________________________________

### Cutting off branches
Consider the following type of trees with values stored in the nodes:

  data Tree a = Node (Tree a) a (Tree a)
              | Leaf
    deriving (Show, Eq) 

Because of lazy evaluation in Haskell, it is possible to construct infinite trees of this type, for example:

  infiniteTree :: Int -> Tree Int
  infiniteTree n = Node (infiniteTree (n+1)) n (infiniteTree (n+1))

This function constructs an infinite tree where the root has label n, the layer beneath that has label n+1, the layer beneath that has label n+2, etc.

Implement a function cutoff :: Int -> Tree a -> Tree a that cuts off all branches of the tree beyond the given depth, by replacing them with Leaf. For example:

  > cutoff 0 (infiniteTree 0)
  Leaf
  > cutoff 1 (infiniteTree 0)
  Node Leaf 0 Leaf
  > cutoff 2 (infiniteTree 0)
  Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)
  > cutoff 3 (infiniteTree 0)
  Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))

##### Library:
```haskell
data Tree a = Node (Tree a) a (Tree a)
            | Leaf
  deriving (Show, Eq) 
  
infiniteTree :: Int -> Tree Int
infiniteTree n = Node (infiniteTree (n+1)) n (infiniteTree (n+1))
```

##### Test:
```haskell
prop_cutoff0 :: Property
prop_cutoff0 = cutoff 0 (infiniteTree 0) === Leaf

prop_cutoff1 :: Property
prop_cutoff1 = cutoff 1 (infiniteTree 0) === Node Leaf 0 Leaf

prop_cutoff2 :: Property
prop_cutoff2 = cutoff 2 (infiniteTree 0) === Node (Node Leaf 1 Leaf) 0 (Node Leaf 1 Leaf)

prop_cutoff3 :: Property
prop_cutoff3 = cutoff 3 (infiniteTree 0) === Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))

```

##### Solution:
```haskell
cutoff :: Int -> Tree a -> Tree a
cutoff n t = case t of
             Leaf -> Leaf
             Node a m b -> if n == 0 then Leaf 
                           else Node (cutoff (n-1) a) m (cutoff (n-1) b)
```

________________________________________________________________________________________________________________________________________________________

### Flattening an infinite tree
Consider the following type of trees with values stored in the nodes:

data Tree a = Node (Tree a) a (Tree a)
            | Leaf
  deriving (Show, Eq) 

Because of lazy evaluation in Haskell, it is possible to construct infinite trees of this type, for example:

infiniteTree :: Int -> Tree Int
infiniteTree n = Node (infiniteTree (n+1)) n (infiniteTree (n+1))

This function constructs an infinite tree where the root has label n, the layer beneath that has label n+1, the layer beneath that has label n+2, etc.

Now implement a function flatten :: Tree a -> [a] that transforms a tree into a list of the labels in the tree, such that each label of an infinite tree occurs at a finite position in the list.

Note. A simple depth-first traversal of the three will not work because it can get stuck on the left subtree of an infinite tree without ever getting to the right subtree!

##### Library:
```haskell
-- import for testing only
import Data.Set (Set)
import qualified Data.Set as Set

data Tree a = Node (Tree a) a (Tree a)
            | Leaf
  deriving (Show, Eq) 

```

##### Solution:
```haskell
flatten :: Tree a -> [a]
flatten t = case t of 
  Leaf -> []
  Node l m r -> (l >>= flatten) ++ [m] ++ (r >>= flatten)
```

