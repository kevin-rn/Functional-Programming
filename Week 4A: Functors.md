### Functor Tree
Define an instance of the Functor class for the following type of binary trees that have data in their nodes:
```data Tree a = Leaf | Node (Tree a) a (Tree a)```

##### Solution:
```haskell
data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Eq)
  
instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node l m r) =  Node (fmap f l) (f m) (fmap f r)
```

________________________________________________________________________________________________________________________________________________________

### A Functor of Expressions

Consider the following type Expr a of arithmetic expressions that contain variables of some type a:
```
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)
```
For example, if we want to represent variables as string we can use the type Expr String. Show how to make this type into an instance of the Functor class.

##### Solution:
```haskell
instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap f (Var a) = Var (f a)
  fmap f (Val a) = Val a
  fmap f (Add a b) = Add (fmap f a) (fmap f b)
```

________________________________________________________________________________________________________________________________________________________

### Zippy lists

There may be more than one way to make a parameterised type into an applicative functor. 
For example, the library ```Control.Applicative``` provides an alternative zippy instance for lists, in which the function pure makes an infinite list of copies of its argument, and the operator ```<*>``` applies each argument function to the corresponding argument value at the same position. 
Complete the given declarations that implement this idea.  

Note: The ZipList wrapper around the list type is required because each type can only have at most one instance declaration for a given class.


##### Solution:
```haskell
newtype ZipList a = Z [a]
  deriving (Show)
  
instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (map g xs)
  
instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z (x : repeat x)

  -- (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]
```

