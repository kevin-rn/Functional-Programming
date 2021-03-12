### Unary natural numbers
The recursive type of (unary) natural numbers is defined as follows:
```haskell
data Nat = Zero | Suc Nat
```
Assignment 1. Define a recursive function ```natToInteger :: Nat -> Integer``` that converts a unary natural number to a Haskell Integer.  
Assignment 2. Define the recursive functions ```add :: Nat -> Nat -> Nat```, ```mult :: Nat -> Nat -> Nat```, and ```pow :: Nat -> Nat -> Nat```.  
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
Assignment 1. Define a function ```occurs :: Ord a => a -> Tree a -> Bool``` that checks if a value occurs in the given search tree. Hint: the standard prelude defines a type ```data Ordering = LT | EQ | GT``` together with a function ```compare :: Ord a => a -> a -> Ordering``` that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value.  
Assignment 2. Define a function ```is_balanced :: Tree a -> Bool``` that checks if the given tree is balanced. Hint: first define a function that returns the number of elements in a tree.  
Assignment 3. Define a function ```flatten :: Tree a -> [a]``` that returns a list that contains all the elements stored in the given tree from left to right.  
Assignment 4. Define a function ```balance :: [a] -> Tree a``` that converts a non-empty list into a balanced tree.  
The functions you define should satisfy ```flatten (balance xs) == xs``` for any list xs.  

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
size (Node l _ r) = size l + size r

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
    (left, half) = splitAt (length xs `div` 2) xs
    right = tail half
    mid = head half

```

_____________________________________________________________________________________________________________________________________________________

### Expressions 
Consider the type of arithmetic expressions involving + and -:
``` data Expr = Val Int | Add Expr Expr | Subs Expr Expr ```
1. Define a higher-order function ```folde :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a``` such that folde f g h replaces each Val constructor in an expression by the function f, each Add constructor by the function g, and each Subs constructor with the function h.
2. Using folde, define a function ```eval :: Expr -> Int``` that evaluates an expression to an integer value.
3. Using folde, define a function ```size :: Expr -> Int``` that calculates the number of values in an expression.

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
_____________________________________________________________________________________________________________________________________________________

### Equality instances

Complete the given instance declarations for the following types:
```data Option a = None | Some a```
```data List a = Nil | Cons a (List a)```
```data Tree a = Leaf a | Node (Tree a) a (Tree a)```

#### Template:
```haskell
instance Eq a => Eq (Option a) where
  ...
  
instance Eq a => Eq (List a) where
  ...
  
instance Eq a => Eq (Tree a) where
  ...
```

### Solution:
```haskell
instance Eq a => Eq (Option a) where
  (Some a) == (Some b) = (a == b)
  None == None =  True
  _ == _ = False
  
instance Eq a => Eq (List a) where
  (Cons a b) == (Cons c d) = (a == c) && (b == d)
  Nil == Nil = True
  _ == _ = False
  
instance Eq a => Eq (Tree a) where
  (Node a b c) == (Node d e f) = (a == d) && (b == e) && (c == f)
  (Leaf a) == (Leaf b) = (a == b)
  _ == _ = False
```
_____________________________________________________________________________________________________________________________________________________

### Tautology checker
You are given a tautology checker for boolean propositions (see Section 8.6 of the book).  
Assignment 1. Extend the tautology checker to support the use of logical disjunction (\/) and equivalence (<=>) of propositions. The new constructors should be called Or and Equiv (otherwise the tests will not work).  
Assignment 2. Implement a function ```isSat :: Prop -> Maybe Subst``` that returns Just s if there is a substitution s for which the given proposition is true, and Nothing if there is no such substitution.  
Assignment 3. Optimize the implementation of isSat so it has a polynomial complexity. (Note: This could be hard.)  

#### Template:
```haskell
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
  deriving (Show)
          
type Assoc k v = [(k,v)]
          
find :: (Eq k) => k -> Assoc k v -> v
find k [] = error "Key not found!"
find k ((k',x):xs)
  | k == k'   = x
  | otherwise = find k xs

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0       = [[]]
bools n | n>0 = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)
  
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = nub (vars p)
  
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
```

#### Solution:
```haskell
data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Equiv Prop Prop
  deriving (Show)
          
type Assoc k v = [(k,v)]
          
find :: (Eq k) => k -> Assoc k v -> v
find k [] = error "Key not found!"
find k ((k',x):xs)
  | k == k'   = x
  | otherwise = find k xs

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Equiv p q) = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)   = vars p ++ vars q
vars (Equiv p q) = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0       = [[]]
bools n | n>0 = map (False:) bss ++ map (True:) bss
  where bss = bools (n-1)
  
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where vs = nub (vars p)
  
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

isSat :: Prop -> Maybe Subst
isSat p = if length list == 0 then Nothing else Just (head list)
  where
    list = filter (\x -> eval x p) (substs p)
```

_____________________________________________________________________________________________________________________________________________________

### Shapes
Define a new typeclass Shape a with the following functions:
 ```corners :: a -> Int
    circumference :: a -> Double
    surface :: a -> Double
    rescale :: Double -> a -> a```
Then, define instances of this typeclass for the following types:
```haskell
data Square = Square { squareSide :: Double }  
data Rectangle = Rect { rectWidth :: Double , rectHeight :: Double }  
data Circle = Circle { circleRadius :: Double }  
```
Optional: also implement instances for the following types:  
```haskell
data Triangle = Triangle { triangleSide1 :: Double, triangleSide2 :: Double, triangleSide3 :: Double }  
data RegularPolygon = Poly { polySides :: Int , polySideLength :: Double }  
```

#### Template:
```haskell
data Square = Square { squareSide :: Double }
  deriving (Show, Eq)
  
data Rectangle = Rect { rectWidth :: Double , rectHeight :: Double }
  deriving (Show, Eq)
  
data Circle = Circle { circleRadius :: Double }
  deriving (Show, Eq)
  
data Triangle = Triangle { triangleSide1 :: Double, triangleSide2 :: Double, triangleSide3 :: Double }
  deriving (Show, Eq)
  
data RegularPolygon = Poly { polySides :: Int , polySideLength :: Double }
  deriving (Show, Eq)
```

#### Solution:
```haskell
data Square = Square { squareSide :: Double }
  deriving (Show, Eq)
  
data Rectangle = Rect { rectWidth :: Double , rectHeight :: Double }
  deriving (Show, Eq)
  
data Circle = Circle { circleRadius :: Double }
  deriving (Show, Eq)
  
data Triangle = Triangle { triangleSide1 :: Double, triangleSide2 :: Double, triangleSide3 :: Double }
  deriving (Show, Eq)
  
data RegularPolygon = Poly { polySides :: Int , polySideLength :: Double }
  deriving (Show, Eq)

class Shape a where
  corners :: a -> Int
  circumference :: a -> Double
  surface :: a -> Double
  rescale :: Double -> a -> a
  
instance Shape Square where
  corners (Square _) = 4
  circumference (Square side) = 4 * side
  surface (Square side) = side * side
  rescale r (Square side) = Square (r * side)

instance Shape Rectangle where
  corners (Rect _ _) = 4
  circumference (Rect width height) = 2 * width + 2 * height
  surface (Rect width height) = width * height
  rescale r (Rect width height) = Rect (r * width) (r * height)
  
instance Shape Circle where
  corners (Circle _) = 0
  circumference (Circle radius) = 2 * pi * radius
  surface (Circle radius) = pi * (radius ^ 2)
  rescale r (Circle radius) = Circle (r * radius)
  
instance Shape Triangle where
  corners (Triangle _ _ _) = 3
  circumference (Triangle side1 side2 side3) = side1 + side2 + side3
  surface (Triangle side1 side2 side3) = sqrt (p * (p - side1) * (p - side2) * (p - side3))
    where
      p = (side1 + side2 + side3) / 2
  rescale r (Triangle side1 side2 side3) = Triangle (r * side1) (r * side2) (r * side3)

instance Shape RegularPolygon where
  corners (Poly sides _) = sides
  circumference (Poly sides sidelength) = (fromIntegral sides) * sidelength
  surface (Poly sides sidelength) = numerator / denominator
    where
      numerator = ((fromIntegral sides) * (sidelength^2))
      denominator = (4 * tan (pi / (fromIntegral sides)))
  rescale r (Poly sides sidelength) = Poly sides (r * sidelength)
```

_____________________________________________________________________________________________________________________________________________________

### Quaternions 
[Quaternions](https://en.wikipedia.org/wiki/Quaternion) are a generalization of complex numbers developed initially by the Irish mathematician Hamilton to solve dynamics problems in physics. More recently, they have been used in computer graphics to efficiently compute transformations in 3D space. Where complex numbers have two components (a real and an imaginary part), quaternions have four. An arbitrary quaternion can be written as a + b*i + c*j + d*k where a,b,c,d are real numbers and i,j,k are constants satisfying the following laws:
- i*i = -1  
- j*j = -1  
- k*k = -1  
- i*j = k  
- j*i = -k  
- j*k = i  
- k*j = -i  
- k*i = j  
- i*k = -j  
Note that multiplication on quaternions is not commutative: i*j is not equal to j*i!  

Your task is to implement a Haskell type Quaternion and define the constants ```i,j,k :: Quaternion```, a function ```fromDouble :: Double -> Quaternion```, and give instances for the Eq, Show, and Num classes. Some further details:
- Quaternions should be pretty-printed in the format 1.2 + 3.4i + 5.6j + 7.8k
- The absolute value of a quaternion equals the square root of the sum of the squares of all its components, i.e. abs(a+bi+cj+dk)=√(a^2+b^2+c^2+d^2)  
- The abs and signum functions should satisfy the equation x = abs x * signum x for any quaternion x.  

### Test:
```haskell
prop_prnt = show (Quaternion 1 2 3 4) === "1.0 + 2.0i + 3.0j + 4.0k"
prop_eq = Quaternion 1 2 3 4 === Quaternion 1 2 3 4
prop_neq = (Quaternion 1 2 3 5 /= Quaternion 1 2 3 4) === True
prop_absi = abs i === fromDouble 1.0
prop_absj = abs j === fromDouble 1.0
prop_absk = abs k === fromDouble 1.0
prop_abs = abs (Quaternion 1 0 0 0) === fromDouble 1.0
prop_abs0 = abs (Quaternion 0 0 0 0) === fromDouble 0.0

prop_i_mult = i * i === Quaternion (-1.0) 0 0 0
prop_j_mult = j * j === Quaternion (-1.0) 0 0 0
prop_k_mult = k * k === Quaternion (-1.0) 0 0 0

prop_ij_mult = i * j === k
prop_ji_mult = j * i === -k
prop_jk_mult = j * k === i
prop_kj_mult = k * j === -i
prop_ki_mult = k * i === j
prop_ik_mult = i * k === -j


instance Arbitrary Quaternion where
   arbitrary = do
     a <- arbitrary
     b <- arbitrary
     c <- arbitrary
     d <- arbitrary
     return $ Quaternion a b c d

prop_abs_signum :: Quaternion -> Property
prop_abs_signum q @ (Quaternion a b c d) = (a /= 0 && b /= 0 && c /= 0 && d /= 0) ==> x < d && y < d && z < d && w < d
  where 
   (Quaternion x y z w) = (abs q * signum q) - q
   d = 0.0001
```

#### Solution:
```haskell
data Quaternion = Quaternion { a :: Double, b :: Double, c :: Double, d :: Double }
  deriving(Eq)

i = Quaternion { a=0, b=1, c=0, d=0 }
j = Quaternion { a=0, b=0, c=1, d=0 }
k = Quaternion { a=0, b=0, c=0, d=1 }
  
fromDouble :: Double -> Quaternion
fromDouble r = Quaternion { a=r, b=0, c=0, d=0 }

instance Show Quaternion where
  show (Quaternion a b c d) = show a ++ " + " ++ show b  ++ "i + " ++ show c  ++ "j + " ++ show d  ++ "k"
  
instance Num Quaternion where

  (+) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion {a= a1 + a2, b = b1 + b2, c = c1 + c2, d =  d1 + d2 } 
  (-) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion {a= a1 - a2, b = b1 - b2, c = c1 - c2, d =  d1 - d2 }
  (*) (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) = Quaternion {a= r, b = i, c = j, d = k } 
    where
      r =  (a1 * a2) - (b1 * b2) - (c1 * c2) - (d1 * d2)
      i = (a1 * b2) + (b1 * a2) + (c1 * d2) - (d1 * c2)
      j = (a1 * c2) - (b1 * d2) + (c1 * a2) + (d1 * b2)
      k =(a1 * d2) + (b1 * c2) - (c1 * b2) + (d1 * a2)
  abs (Quaternion a b c d) =  fromDouble (sqrt (a ** 2 + b ** 2 + c ** 2 + d ** 2))
  signum (Quaternion a1 b1 c1 d1) = Quaternion { a = signum a1, b = signum b1, c = signum c1, d = signum d1 }
  negate (Quaternion a1 b1 c1 d1) = Quaternion { a = negate a1, b = negate b1, c = negate c1, d = negate d1 }
  fromInteger r = fromDouble (fromIntegral r)

```
_____________________________________________________________________________________________________________________________________________________

### Pretty-printing JSON data
The JSON (JavaScript Object Notation) language is a small, simple representation for storing and transmitting structured data, for example over a network connection. It is most commonly used to transfer data from a web service to a browser-based JavaScript application. The JSON format is described at www.json.org, and in greater detail by [RFC 4627](https://www.ietf.org/rfc/rfc4627.txt).  

JSON supports four basic types of value: strings, numbers, booleans, and a special value named null. The language provides two compound types: an array is an ordered sequence of values, and an object is an unordered collection of name/value pairs. The names in an object are always strings; the values in an object or array can be of any type.  

To work with JSON data in Haskell, we use an algebraic data type to represent the range of possible JSON types.  

Exercise 1. Define a datatype JValue with constructors JString (storing a String), JNumber (storing a Double), JBool (storing a Bool), JNull, JObject (storing a list of key-value pairs), and JArray (storing a list of values). Add deriving Show to the end of your definition to derive a Show instance for your type.  
Exercise 2. Implement an instance of the Eq class for JValue.  
We can see how to use a constructor to take a normal Haskell value and turn it into a JValue. To do the reverse, we use pattern matching.  

Exercise 3. Implement the following functions for converting JSON values to Haskell values:
```
    getString :: JValue -> Maybe String
    getInt :: JValue -> Maybe Int
    getDouble :: JValue -> Maybe Double
    getBool :: JValue -> Maybe Bool
    getObject :: JValue -> Maybe [(String, JValue)]
    getArray :: JValue -> Maybe [JValue]
    isNull :: JValue -> Bool
```
Hint. The function toInt should round the given number down to the nearest integer. For this, you can use the function truncate.

Now that we have a Haskell representation for JSON’s types, we’d like to be able to take Haskell values and render them as JSON data.
Exercise 4. Implement a function ```renderJValue :: JValue -> String``` that prints a value in JSON form (see “Tests”).

Note that when pretty printing a string value, JSON has moderately involved escaping rules that we must follow. For this exercise, you can approximate the escaping rules by using ```show``` on the string. This will use the Haskell escaping rules rather than the JSON escaping rules, which is good enough for the tests of this exercise. For the full project you will need to implement the proper JSON escaping rules, however.

(This assignment is based on the material from [Chapter 5 of Real World Haskell](http://book.realworldhaskell.org/read/writing-a-library-working-with-json-data.html), which is licensed under a Attribution-NonCommercial 3.0 Unported Creative Commons license.)

### Tests:
```haskell
test_string :: JValue
test_string = JString "foo"

test_number :: JValue
test_number = JNumber 2.7

test_array :: JValue
test_array = JArray [JNumber (-3.14), JBool True, JNull, JString "a string"]

test_object :: JValue
test_object = JObject [ ("numbers", JArray [JNumber 1, JNumber 2, JNumber 3, JNumber 4, JNumber 5])
                      , ("useful", JBool False)
                      ]
                      
prop_render_string :: Bool
prop_render_string = renderJValue test_string == "\"foo\""

prop_render_number :: Bool
prop_render_number = renderJValue test_number == "2.7"

prop_render_bool :: Bool
prop_render_bool = renderJValue (JBool True) == "true"

prop_render_array :: Bool
prop_render_array = renderJValue test_array == "[-3.14, true, null, \"a string\"]"

prop_render_object :: Bool
prop_render_object = renderJValue test_object == "{\"numbers\": [1.0, 2.0, 3.0, 4.0, 5.0], \"useful\": false}"

```

#### Solution:
```haskell
data JValue = JString { string :: String }
            | JNumber { double :: Double }
            | JBool { boolean :: Bool } 
            | JNull
            | JObject { pairs :: [(String, JValue)]}
            | JArray { list :: [JValue] }
            deriving(Show)

-- Can also use deriving(Eq) isntead of creating an instance for Eq
instance Eq JValue where
  (JString a) == (JString b) = a == b
  (JNumber a) == (JNumber b) = a == b
  (JBool a) == (JBool b) = a == b
  (JObject a) == (JObject b) = a == b
  (JArray a) == (JArray b) = a == b
  JNull == JNull = True 
  _ == _ = False
  
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _  = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _ = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _ = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull

renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JArray a) = "[" ++ (intercalate ", " (map renderJValue a)) ++ "]"
renderJValue (JObject o) = "{" ++ (intercalate ", " (map renderPair o)) ++ "}"
  where 
    renderPair (k,v)   = show k ++ ": " ++ renderJValue v

```


