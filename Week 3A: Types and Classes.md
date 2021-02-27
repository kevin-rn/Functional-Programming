### Unary natural numbers
The recursive type of (unary) natural numbers is defined as follows:
```haskell
data Nat = Zero | Suc Nat
```
Assignment 1. Define a recursive function natToInteger :: Nat -> Integer that converts a unary natural number to a Haskell Integer.  
Assignment 2. Define the recursive functions add :: Nat -> Nat -> Nat, mult :: Nat -> Nat -> Nat, and pow :: Nat -> Nat -> Nat.  
  Hint: make use of the functions you already defined.  
Assignment 3. Use a deriving clause to automatically define instances of the Show and Eq typeclasses for the Nat type.  
Assignment 4. Define an instance of the Ord typeclass for the Nat type. The instance declaration should look as follows:  
```haskell
instance Ord Nat where
  -- (<=) :: Nat -> Nat -> Bool
  x <= y = ...
```
Assignment 5. Define an instance of the Num typeclass for the Nat type. The instance declaration should look as follows:
```haskell
instance Num Nat where
  -- (+) :: Nat -> Nat -> Nat
  x + y = ...

  -- (*) :: Nat -> Nat -> Nat
  x * y = ...

  -- fromInteger :: Integer -> Nat
  fromInteger x = ...
```
You do not need to give definitions for the functions abs, signum, and negate. Hint. Make use of the add and mult functions you defined before.

#### Template:
```haskell
data Nat = Zero | Suc Nat 

natToInteger :: Nat -> Integer
natToInteger n = undefined

add :: Nat -> Nat -> Nat
add m n = undefined

mult :: Nat -> Nat -> Nat
mult m n = undefined

pow :: Nat -> Nat -> Nat
pow m n = undefined

instance Ord Nat where
  -- (<=) :: Nat -> Nat -> Bool
  x <= y = undefined

instance Num Nat where
  -- (+) :: Nat -> Nat -> Nat
  x + y = undefined

  -- (*) :: Nat -> Nat -> Nat
  x * y = undefined

  --- fromInteger :: Integer -> Nat
  fromInteger x = undefined
```

#### Solution:
```haskell
data Nat = Zero | Suc Nat
  deriving (Show, Eq)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Suc n) = 1 + natToInteger n

add :: Nat -> Nat -> Nat
add Zero n = n
add m Zero = m
add a (Suc b) = Suc (add a b)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Suc m) n = add n (mult m n)

pow :: Nat -> Nat -> Nat
pow _ Zero = (Suc Zero)
pow a (Suc Zero) = a
pow m (Suc n) = mult m (pow m n) 

instance Ord Nat where
  -- (<=) :: Nat -> Nat -> Bool
  x <= y = (natToInteger x) <= (natToInteger y)

instance Num Nat where
  -- (+) :: Nat -> Nat -> Nat
  x + y = add x y

  -- (*) :: Nat -> Nat -> Nat
  x * y = mult x y

--fromInteger :: Integer -> Nat
  fromInteger x = if x == 0 then Zero 
                  else Suc (fromInteger (x-1))
```
_____________________________________________________________________________________________________________________________________________________


### Binary search trees
Consider the following type of binary trees:
```hasekll
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
```
A binary tree is a search tree if for every node, all values in the left subtree are smaller than the stored value, and all values in the right subtree are greater than the stored value.  
A tree is balanced if the number of leaves in the left and right subtree of every node differs by at most one.  
Assignment 1. Define a function occurs :: Ord a => a -> Tree a -> Bool that checks if a value occurs in the given search tree. Hint: the standard prelude defines a type data Ordering = LT | EQ | GT together with a function compare :: Ord a => a -> a -> Ordering that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value.  
Assignment 2. Define a function is_balanced :: Tree a -> Bool that checks if the given tree is balanced. Hint: first define a function that returns the number of elements in a tree.  
Assignment 3. Define a function flatten :: Tree a -> [a] that returns a list that contains all the elements stored in the given tree from left to right.  
Assignment 4. Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree.  
The functions you define should satisfy flatten (balance xs) == xs for any list xs.  

#### Solution:
```haskell
occurs :: Ord a => a -> Tree a -> Bool
occurs a Empty = False
occurs a (Leaf n) = compare a n == EQ
occurs a (Node l m r) = case compare a m of
                             LT -> occurs a l
                             EQ -> True
                             GT -> occurs a r
  
size :: Tree a -> Int
size Empty = 0
size (Leaf _) = 1
size (Node l _ r) = size l + 1 + size r

is_balanced :: Tree a -> Bool
is_balanced Empty = True
is_balanced (Leaf _) = True
is_balanced (Node l m r) = abs (size l - size r) <= 1 && is_balanced l && is_balanced r

flatten :: Tree a -> [a]
flatten Empty = []
flatten (Leaf x) = [x]
flatten (Node l m r) = (flatten l) ++ [m] ++ (flatten r) 

balance :: [a] -> Tree a
balance [] = Empty
balance [n] = Leaf n
balance xs = Node (balance left) mid (balance right)
  where 
    (left, half) = splitAt ((length xs `div` 2) - 1) xs
    right = tail half
    mid = head half

```

_____________________________________________________________________________________________________________________________________________________

### Expressions 
Consider the type of arithmetic expressions involving + and -:
```haskell data Expr = Val Int | Add Expr Expr | Subs Expr Expr ```
1. Define a higher-order function folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a such that folde f g h replaces each Val constructor in an expression by the function f, each Add constructor by the function g, and each Subs constructor with the function h.
2. Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value.
3. Using folde, define a function size :: Expr -> Int that calculates the number of values in an expression.

```haskell
folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
folde f g h (Val n) = f n
folde f g h (Add x y) = g (folde f g h x) (folde f g h y)
folde f g h (Subs x y) = h (folde f g h x) (folde f g h y)

-- use as functions f g h the normal haskell functions id + -
eval :: Expr -> Int
eval e = folde id (+) (-) e

-- subsitute Val with 1 and Add, Subst with + to count all Val
size :: Expr -> Int
size e = folde (\_ -> 1) (+) (+) e
```




