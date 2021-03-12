### Loop-i-doop
Implement the function loop :: Monad m => m () -> m () that takes as input a monadic action and keeps repeating this action forever. For example:

    > loop (putStrLn "spam")
    spam
    spam
    spam
    spam
    spam
    ...

Note. This function is called forever in the Haskell Prelude.

##### Template:
```haskell
loop :: Monad m => m () -> m ()
loop = undefined
```

##### Solution:
```haskell
loop :: Monad m => m () -> m ()
loop a = a >> loop a
```

_____________________________________________________________________________________________________________________________________________________________

### Sequencing data
Reimplement the library function sequence :: Monad m => [m a] -> m [a] that takes a list of monadic actions, and evaluates them in left-to-right sequence, collecting all the results into a list.  

##### Template:
```haskell
sequence :: Monad m => [m a] -> m [a]
sequence = undefined
```

##### Test:
```haskell
prop_sequence_example1 :: Property
prop_sequence_example1 = sequence [Just 1, Just 2, Just 3] === Just [1,2,3]

prop_sequence_example2 :: Property
prop_sequence_example2 = sequence [Left "oops!", Right 42, Left "oh no..."] === Left "oops!"
```

##### Solution:
```haskell
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (x:xs) = x >>= \v -> sequence xs >>= \vs -> return (v:vs)
```

_____________________________________________________________________________________________________________________________________________________________

### Monadic filter
Reimplement the library function filterM :: Monad m => (a -> m Bool) -> [a] -> m [a], that takes a (monadic) predicate a -> m Bool and uses this to filter a given list.

Note. filterM must process the list elements left-to-right, and it must preserve the order of the elements of the input list, as far as they appear in the result.  

##### Template:
```haskell
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM = undefined
```

##### Test:
```haskell
-- Keeping all the divisors of a given number. If any division by 0 happens, the whole thing becomes `Nothing`
prop_filterM_divisors :: Property
prop_filterM_divisors = filterM (isDivisorOf 10) [1..10] === Just [1,2,5,10]
  where
    isDivisorOf x y | y == 0    = Nothing
                    | otherwise = Just (x `mod` y == 0)
                    
prop_filterM_divisors_error :: Property
prop_filterM_divisors_error = filterM (isDivisorOf 10) [0..10] === Nothing
  where
    isDivisorOf x y | y == 0    = Nothing
                    | otherwise = Just (x `mod` y == 0)
```

##### Solution:
```haskell
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = pure []
filterM f (x:xs) = do
  fx <- f x
  if fx 
    then do 
      fx' <- filterM f xs
      pure (x:fx')
    else filterM f xs

```

_____________________________________________________________________________________________________________________________________________________________

### A functional while loop
Some algorithms are expressed more naturally as an imperative while loop instead of as a recursive function. Implement a monadic function while :: Monad m => (m Bool) -> m () -> m () that takes as arguments a loop condition cond, and a loop body body, and repeatedly runs the loop body as long as the condition returns True.

As an example of how this function while might be used, the test template contains an implementation of Euclid’s algorithm euclid :: Int -> Int -> Int for finding the greatest common divisor of two positive numbers, using the State monad with a state of type (Int,Int).  

##### Template:
```haskell
while :: Monad m => (m Bool) -> m () -> m ()
while loopCond loopBody = undefined
```

##### Solution:
```haskell

```

_____________________________________________________________________________________________________________________________________________________________

### The Reader monad
The Reader type captures the effect of a global read-only variable. It is defined as follows:

newtype Reader r a = Reader (r -> a)

    runReader :: Reader r a -> r -> a
    runReader (Reader f) = f

There are two functions that are often useful when working with the Reader type. The first one is asks, which gets the value of the global variable and
applies a given function to it:

    asks :: (r -> a) -> Reader r a
    asks f = Reader f

The second function is local, which allows running a Reader action with a
different value of the local variable.

    local :: (r -> r) -> Reader r a -> Reader r a
    local g (Reader f) = Reader (f . g)

Here is an example of how to use the Reader monad:

    reader_example :: Reader Int (Int,Int)
    reader_example = do
      x <- asks (*5)        -- get current value of global variable multiplied by 5
      y <- asks (+3)        -- get current value of global variable plus 3
      z <- local (+1) $ do  -- locally add 1 to the global variable
        a <- asks (*5)
        b <- asks (+3)
        return (a+b)
      return (x+y, z)

We can run this example with the value 1 for the global variable as follows: runReader reader_example 1. This returns the result (9,15).

Your assignment is to complete the given instance declarations to make Reader into an instance of Functor, Applicative, and Monad. For implementing the Monad instance, the helper function runReader :: Reader r a -> r -> a may be useful.  

##### Template:
```haskell
instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader g) = undefined
  
instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = undefined
  
  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader f) <*> (Reader g) = undefined
  
instance Monad (Reader r) where
  -- return :: a -> Reader r a
  return   = undefined
  
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  mx >>= f = undefined
```


##### Test:
```haskell
-- An example of using the Reader monad
reader_example :: Reader Int (Int,Int)
reader_example = do
  x <- asks (*5)        -- get current value of global variable multiplied by 5
  y <- asks (+3)        -- get current value of global variable plus 3
  z <- local (+1) $ do  -- locally add 1 to the global variable
    a <- asks (*5)
    b <- asks (+3)
    return (a+b)
  return (x+y, z)
  
prop_reader_example :: Property
prop_reader_example = runReader reader_example 1 === (9,15)
```


##### Library:
```haskell
newtype Reader r a = Reader (r -> a)

runReader :: Reader r a -> r -> a
runReader (Reader f) = f

-- The asks function gets the value of the global variable and
-- applies the given function to it.
asks :: (r -> a) -> Reader r a
asks f = Reader f

-- The local function allows running a Reader action with a
-- different value of the local variable.
local :: (r -> r) -> Reader r a -> Reader r a
local g (Reader f) = Reader (f . g)

```

##### Solution:
```haskell

```

_____________________________________________________________________________________________________________________________________________________________

### The Writer monad

The Writer type captures the effect of being able to output values of some type w. It is defined as follows:

    newtype Writer w a = Writer (w,a)

Complete the given instance declaration to make Writer w into a functor.

In order to make Writer w into an instance of Applicative and Monad, we need some way to combine the outputs when a computation of type Writer w a produces multiple outputs. We do this by requiring that w is an instance of the Semigroup typeclass, so that we can use (<>) to combine outputs. We also need a default output for the cases where the action does not output anything. For this we require that w is also an instance of Monoid, so we can use mempty as the default value.

Now, define an instance of the Applicative class for the Writer w type.

Finally, define an instance of the Monad class for Writer w.

Hint. It may help you to first define the helper function runWriter :: Writer w a -> (w,a).  

##### Template:
```haskell
instance Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer (w,x)) = undefined
  
instance Monoid w => Applicative (Writer w) where
  -- pure :: a -> Writer w a
  pure x = undefined
  
  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  wf <*> wx = undefined
  
instance Monoid w => Monad (Writer w) where
  -- return :: a -> Writer w a
  return   = undefined
  
  -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
  mx >>= f = undefined
```

##### Library:
```haskell
newtype Writer w a = Writer (w, a)
```

##### Solution:
```haskell

```

_____________________________________________________________________________________________________________________________________________________________

### Expr monad

Consider the following type Expr a of arithmetic expressions that contain variables of some type a:

    data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
      deriving (Show)

For example, if we want to represent variables as string we can use the type Expr String. The library code (not visible) defines this datatype and an instance of the Functor typeclass. Show how to make this type into an instance of the classes Applicative and Monad.

Hint. It may be easier to implement the Monad instance first and derive the implementation of Applicative after. Intuitively, the behaviour of e >>= f is to replace each variable in the expression e with a new expression, which is produced by applying the function f to the variable. For example:

    > let f "x" = Val 1; f "y" = Add (Val 1) (Var "z")
    > Add (Var "x") (Var "y") >>= f
    Add (Val 1) (Add (Val 1) (Var "z"))


##### Template:
```haskell
instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = undefined
  
  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  fe <*> xe = undefined
  
instance Monad Expr where
  -- return :: a -> Expr a
  return = pure
  
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  e >>= f = undefined
```


##### Test:
```haskell
prop_example :: Property
prop_example = (Add (Var "x") (Var "y") >>= f) === (Add (Val 1) (Add (Val 1) (Var "z")))
  where
    f "x" = Val 1
    f "y" = Add (Val 1) (Var "z")
    
prop_bind_var :: Property
prop_bind_var = (Var "x" >>= \_ -> Add (Var "y") (Var "z")) === Add (Var "y") (Var "z")
```

##### Solution:
```haskell

```
