### Question 1: Theory Question
Suppose that you have been invided to write an article for a professional computing magazine on the benefits that pure functional programming brings to programmers.   
Give a list of at least 5 benefits that you would mention, and illustrate each of the benefits with a small example written in Haskell.  

- no side effects to any function, easier to contain effects of a function e.g. 

x = 1

foo :: Int -> Int
foo a = a + 1

x = f x will not change the value of the initial x and give an error

- some functions are better implemented in a functional form, eg: quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort1 [a | a <- xs, a <= x]
      biggerSorted = quicksort1 [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

- recursive functions are easier in the functional paradigm, e.g.
loop :: a -> [a]
loop a = a : loop a

- pattern matching, e.g.

foo : [a] -> [a]
foo [] = a
foo (x:xs) = x : foo xs

- Easier to add new datatypes, but can be difficult to extend existing ones
- support for lazy evaluation, allowing for infinite data structures,  e.g. head [1..] returns 1
- The use of list comphrehension makes things easier as looping isn’t needed anymore to create lists. e.g. [1..100] for a list of ints ranging 1 to 100
- Higher order functions as its possible to pass one function onto another and also enables reusing other functions for example map (+1) [1..5] will add 1 to every element of the list

____________________________________________________________________________________________________________________________________________________________________

### Question 2: Multiple Choice Questions
```haskell
1. Which of the following statements is true for all finite lists xs and functions f and g of the right types?
[ ] `map f (map g xs) = map g (map f xs)     
[ ] reverse (reverse xs) = reverse xs    
[ ] reverse xs = xs   
[ ] map f (map f xs) = map f xs    
[x] reverse (map f xs) = map f (reverse xs)

2. A function of type (Int -> Int) -> Int:
[ ] Takes two arguments as inputs. 
[ ] Takes a pair of arguments as input. 
[ ] Returns a pair of results. 
[ ] Returns a function as a result. 
[x] Takes a function as its argument.

3. Which of the following equations does NOT hold for all functions f and non-negative numbers n?
[ ] map f . reverse = reverse . map f   
[x] map f . sort = sort . map f]  
[ ] map f . drop n = drop n . map f  
[ ] map f . take n = take n . map f

4. Which of the following statements about Haskell is false:
[ ] Function application associates to the left.   
[ ] All type errors are detected at compile time.    
[x] All functions are guaranteed to terminate.  
[ ] Recursive functions can have more than one base case.

5. How many distinct elements are there in the Agda type Vec Bool 3?
0 - 1 - 2 - 3 - 6 - [8] - 9
```
____________________________________________________________________________________________________________________________________________________________________

### Question 3: Defining and testing functions
Implement a function giveChange :: Int -> [Int] -> [Int] such that giveChange total coins returns the shortest sublist coins' of coins such that sum coins' = total.   
You may assume that both total and all coins are greater than 0, and that there is at least one possible solution.  
Also write three QuickCheck tests for the function you just implemented, based on the following properties:  
- The sum of the numbers in the result of giveChange n xs is equal to n.  
- When there is a coin with value exactly n at some position in the list, then the result of giveChange n coins has length 1.  
- When all the coins have value 1, the result of giveChange n coins is a list of n times the number 1  

##### Test:
```haskell
prop_eq_sum :: Int -> [Int] -> Property
prop_eq_sum n xs = (n > 0) && not (null xs) && not (null (subsetshelper n xs)) ==> sum (giveChange n xs) == n

prop_contains :: Int -> [Int] -> Property
prop_contains n xs = (n > 0) && (n `elem` xs) && not (null (subsetshelper n xs)) ==> length (giveChange n xs) == 1

prop_only_ones :: Int -> [Int] -> Property
prop_only_ones n xs = (n > 0) && checkOnes xs && not (null (subsetshelper n xs)) ==> giveChange n xs == replicate n 1

checkOnes :: [Int] -> Bool
checkOnes [] = True
checkOnes (x:xs) = if x == 1 then checkOnes xs else False

subsetshelper :: Int -> [Int] -> [[Int]]
subsetshelper total coins = [n | n <- helper $ sort coins, sum n == total]
  where
    helper [] = [[]]
    helper (x:xs) = map (x:) (helper xs) ++ helper xs
```

##### Solution:
```haskell
import           Data.List

-- very inefficient
giveChange :: Int -> [Int] -> [Int]
giveChange total []    = []
giveChange total coins = head $ sortBy
    (\xs ys -> compare (length xs) (length ys))
    [ sl | sl <- subsequences coins, sum sl == total ]
    
-- Alternative notation
giveChange :: Int -> [Int] -> [Int] 
giveChange total coins = findMinlist (subsets total coins)

findMinlist :: [[Int]] -> [Int]
findMinlist [] = []
findMinlist [x] = x
findMinlist (x:y:xs)
    | (length x) < (length y) = findMinlist (x:xs)
    | otherwise = findMinlist (y:xs)
    
subsets :: Int -> [Int] -> [[Int]]
subsets total coins = [n | n <- helper $ sort coins, sum n == total]
  where
    helper [] = [[]]
    helper (x:xs) = map (x:) (helper xs) ++ helper xs
```

____________________________________________________________________________________________________________________________________________________________________

### Question 4: Data types
Consider the following datatype of trees: data Tree = Leaf Int | Node Tree Tree
- Write down three different values of type Tree with the property that all the leaves in each example contain the number 0.
- Define the functions size :: Tree -> Int (computing the number of leaves and nodes in the tree) and depth :: Tree -> Int (computing the longest sequence of nodes that can be taken from the root before reaching a leaf).
- A tree is full if each node has the property that the size of the left and right subtree is equal. Implement a function isFull :: Tree -> Bool that decides if a tree is full.
- Implement a function fullTree :: Int -> Tree that produces a full tree of a given depth in which each leaf contains the number 0.

##### Solution:
```haskell
data Tree = Leaf Int | Node Tree Tree

tree1, tree2, tree3 :: Tree
tree1 = Leaf 0
tree2 = Node (Leaf 0) (Leaf 1)
tree3 = Node (Leaf 0) (Node (Leaf 1) (Leaf 2))

size :: Tree -> Int
size (Leaf _  ) = 1
size (Node l r) = 1 + size l + size r

depth :: Tree -> Int
depth (Leaf _  ) = 0
depth (Node l r) = 1 + max (depth l) (depth r)

isFull :: Tree -> Bool
isFull (Leaf _) = True
isFull (Node l r) = (size l) == (size r) && (isFull l) && (isFull r)

fullTree :: Int -> Tree
fullTree 0 = Leaf 0
fullTree n = Node (fullTree (n - 1)) (fullTree (n - 1))
```

____________________________________________________________________________________________________________________________________________________________________

### Question 5: Functors and Applicatives
Consider the following datatype representing an infinite two-dimensional grid of numbers:
```haskell
data Grid a 
  = EmptyGrid
  | FullGrid ((Int,Int) -> a)
```
A grid is either empty, or it contains a value of type a for each coordinate of type (Int,Int).  
Define instances of Functor and Applicative for the Grid datatype.  
In a comment, argue why your implementation satisfies the laws of Functor and the first law of Applicative.  

##### Solution:
```haskell
data Grid a
  = EmptyGrid
  | FullGrid ((Int,Int) -> a)

instance Functor Grid where
    -- fmap :: (a -> b) -> Grid a -> Grid b
    fmap f EmptyGrid    = EmptyGrid
    fmap f (FullGrid g) = FullGrid (f . g)

instance Applicative Grid where
    -- pure :: a -> Grid a
    pure x = FullGrid (\_ -> x)

    -- (<*>) :: Grid (a -> b) -> Grid a -> Grid b
    EmptyGrid    <*> _            = EmptyGrid
    _            <*> EmptyGrid    = EmptyGrid
    (FullGrid f) <*> (FullGrid g) = FullGrid (\c -> f c (g c))

-- Cannot be a Monad, see https://stackoverflow.com/c/tud-cs/questions/5482
instance Monad Grid where
    -- return :: a -> Grid a
    return = pure

    -- (>>=) :: Grid a -> (a -> Grid b) -> Grid b
    EmptyGrid    >>= f = EmptyGrid
    (FullGrid g) >>= f = FullGrid
        (\c -> case f (g c) of
            EmptyGrid  -> error "not possible"
            FullGrid h -> h c
        )


{- The Functor instance for Grid satisfies the first Functor law because ...

The first Functor law states that fmap id = id.

EmptryGrid case holds since:
fmap id EmptyGrid = EmptyGrid
id EmptyGrid = EmptyGrid

fmap id (FullGrid func) is the same as FullGrid (id . func) and since id . func = func
it holds that fmap id (FullGrid func) = FullGrid func

-}


{- The Functor instance for Grid satisfies the second Functor law because ...

The second functor law states that fmap (f . g) xs = (fmap f . fmap g) xs for any xs.
EmptyGrid case also holds since fmap (f . g) EmptyGrid = EmptyGrid
(fmap f) . (fmap g) EmptyGrid = fmap f (fmap g EmptyGrid) = fmap f EmptyGrid = EmptyGrid

fmap (f.g) (FullGrid x) = FullGrid ((f.g) . x) = FullGrid (f.g.x)
(fmap f) . (fmap g) (FullGrid x) = fmap f (fmap g (FullGrid x)) = fmap f (FullGrid (g.x)) = FullGrid (f.g.x)


-}


{- The Functor instance for Grid satisfies the first Applicative law because ...

[WRITE YOUR ANSWER HERE]

The first Applicative law states that pure id <*> x = x.
So pure id <*> EmptyGrid = (FullGrid id) <*> EmptyGrid = EmptyGrid holds.
And pure id <*> (FullGrid func) = (FullGrid id) <*> (FullGrid func) = FullGrid (id . func) = FullGrid func


-}
```

____________________________________________________________________________________________________________________________________________________________________

### Question 6: Monads

In the library code, the State monad is defined.   
First, implement a function fresh :: State Int Int that increments the value of the state by 1 and returns the old value of the state.   
Next, use this function to define a function replace :: [Int] -> State Int [Int] that replaces each occurrence of 0 in a list by a fresh integer, using the state to keep track of what integer to use next.   
For example: runState (replace [5,0,12,0,0,20]) 1 == ([5,1,12,2,3,20], 4).

For this question, you should make use of monadic syntax (either >>= or do-notation), and not pattern match directly on the State constructor.

##### Library:
```haskell
newtype State s a = State (s -> (a,s))

runState :: State s a -> s -> (a,s)
runState (State f) = f

instance Functor (State s) where
  fmap f (State h) = State $ \s -> let (a, newState) = h s
                                   in  (f a, newState)
                                   
instance Applicative (State s) where
  pure x = State $ \s -> (x,s)
  State g <*> State h = State $ \s -> let (f, s')  = g s
                                          (x, s'') = h s'
                                      in  (f x, s'')

instance Monad (State s) where  
  return x = State $ \s -> (x,s)  
  (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                      (State g) = f a  
                                  in  g newState  
-- Get the current state
getState :: State s s
getState = State (\x -> (x,x))

-- Overwrite the state with a new value
putState :: s -> State s ()
putState x = State (\_ -> ((),x))
```

##### Test:
```haskell
prop_example :: Bool
prop_example = runState (replace [5,0,12,0,0,20]) 1 == ([5,1,12,2,3,20], 4)
```

##### Solution:
```haskell
fresh :: State Int Int
fresh = State (\s -> (s, s + 1))

-- version without monadic syntax
-- replace :: [Int] -> State Int [Int]
-- replace []       = return []
-- replace (x : xs) = (:) <$> (if x == 0 then fresh else pure x) <*> replace xs

-- version with monadic syntax
replace :: [Int] -> State Int [Int]
replace []       = return []
replace (x : xs) = do
    y  <- (if x == 0 then fresh else return x)
    ys <- replace xs
    return $ y : ys
    
    
--- Alternative

fresh :: State Int Int
fresh = do
        s <- getState
        putState (s + 1)
        return s

replace :: [Int] -> State Int [Int]
replace [] = State (\s -> ([], s))
replace (x:xs) = State (\old -> let 
                        (f, st1) = if x == 0 then runState fresh old
                                   else runState getState x
                        (fs, st2) = if x /= 0 then runState (replace xs) old
                                   else runState (replace xs) st1
                        in (f:fs, st2))
```

____________________________________________________________________________________________________________________________________________________________________

### Question 7: Laziness
Consider the expression head (filter p xs) where xs is a list of length n. How many times is the function p evaluated under call-by-name and under call-by-value evaluation of this expression? Does this number depend on the contents of the list or only on its length?

Now give an expression that is equivalent to head (filter p xs) but will evaluate p the same number of times under both strategies.

##### Solution
```haskell
xs = [1,2,3]
n = 3

-- Call by value:
head (filter (> 2) xs) =
head (filter (> 2) [1,2,3]) =
head (filter (> 2) [2,3]) =
head (filter (> 2) [3]) =
head ([3]) = 3
-- n times in total

Call by name:
head (filter (> 2) xs) =
head (filter (> 2) [1,2,3]) =
head (filter (> 2) [2,3]) =
head (filter (> 2) [3]) =
head ([3]) = 3
-- amount of times until the first element in the list satisfies the predicate p. The number depends on the content of the list, since the predicate p may be dependent on the elements in list xs. So call-by-name will evaluate the predicate p until some elements satisfies the predicate p.


head (filter p xs) = head (dropWhile (not . p) xs)

case find p xs of
  Just x -> x
  Nothing -> error "yeet"
```

____________________________________________________________________________________________________________________________________________________________________

### Question 8: The Curry-Howard correspondence
Translate the following propositions to Agda types using the Curry-Howard correspondence:
If (not A) or (not B) then not (A and B)
If not (A and B) then (not A) or (not B)
Prove the first statement by implementing an Agda function of the translated type. Explain why it is not possible to prove the second statement in Agda.  

##### Library:
```haskell
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B
infixr 4 _,_

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y

data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

cases : {A B C : Set} → Either A B → (A → C) → (B → C) → C
cases (left x)  f g = f x
cases (right y) f g = g y

data ⊤ : Set where tt : ⊤

data ⊥ : Set where
```

##### Solution
```haskell
open import library

proof1 : {A B : Set} → Either (A → ⊥) (B → ⊥) -> (A × B) → ⊥
proof1 (left f) (a , b) = f a
proof1 (right f) (a , b) = f b

--- Alternative

proof1 : {A B : Set} → (Either (A → ⊥) (B → ⊥)) → ((A × B) → ⊥)
proof1 (left fa) = λ {(a , _) -> (fa a)} 
proof2 (right fb) = λ {(_ , b) -> (fb b)} 

proof2 : {A B : Set} → ((A × B) → ⊥) → Either (A → ⊥) (B → ⊥)
proof2 f = ? -- You can leave this one open

{- It is not possible to prove the second statement in Agda because...

Since the empty case does not take any constructors, proving non-constructive statements are not possible. Agda uses constructive logic, therefore given a proof of not (A and B)
but A and B being unknown it is impossible to derive which one of either A or B holds and which one doesn't.
Thus, if the result of (A and B) is a black-box because of being paired to the 'not' statement, we can not infer the values of A and B themselves. 
Therefore, the statement of proof2 is not provable in Agda.

-}
```

### Question 9: Equational Reasoning
Using the identity type and equational reasoning in Agda, write down the statement and a proof that for any two booleans b1 and b2 and values x, y, z : A,   
we have that if b1 then (if b2 then x else y) else z is equal to if (b1 && b2) then x else (if b1 then y else z).

##### Library:
```haskell
data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

_&&_ : Bool → Bool → Bool
true  && b2 = b2
false && b2 = false

_||_ : Bool → Bool → Bool
true  || b2 = true
false || b2 = b2

if_then_else_ : {A : Set} → Bool → A → A → A
if true   then x  else y  = x
if false  then x  else y  = y

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 4 _≡_

-- symmetry of equality
sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- transitivity of equality
trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- congruence of equality
cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

begin_ : {A : Set} → {x y : A} → x ≡ y → x ≡ y
begin p = p

_end : {A : Set} → (x : A) → x ≡ x
x end = refl

_=⟨_⟩_ : {A : Set} → (x : A) → {y z : A}
       → x ≡ y → y ≡ z → x ≡ z
x =⟨ p ⟩ q = trans p q

_=⟨⟩_ : {A : Set} → (x : A) → {y : A} → x ≡ y → x ≡ y
x =⟨⟩ q = x =⟨ refl ⟩ q

infix   1  begin_
infix   3  _end
infixr  2  _=⟨_⟩_
infixr  2  _=⟨⟩_
```

##### Solution:
```haskell
open import library

proof : {A : Set} (b1 b2 : Bool) (x y z : A) → if b1 then (if b2 then x else y) else z ≡ if (b1 && b2) then x else (if b1 then y else z)
proof true true x y z =
  begin
    if true then (if true then x else y) else z
  =⟨⟩
    if true then x else y
  =⟨⟩
    x
  =⟨⟩
    if true then x else (if true then y else z)
  =⟨⟩
    if (true && true) then x else (if true then y else z)
  end
proof true false x y z =
  begin
    if true then (if false then x else y) else z
  =⟨⟩
    if false then x else y
  =⟨⟩
    y
  =⟨⟩
    if true then y else z
  =⟨⟩
    if false then x else (if true then y else z)
  =⟨⟩
    if (true && false) then x else (if true then y else z)
  end
proof false b x y z =
  begin
    if false then (if b then x else y) else z
  =⟨⟩
    z
  =⟨⟩
    if false then y else z
  =⟨⟩
    if false then x else (if false then y else z)
  =⟨⟩
    if (false && b) then x else (if false then y else z)
  end

```
