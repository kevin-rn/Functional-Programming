
data Greeting : Set where
  hello : Greeting
  goodbye : Greeting
-- ^ Haskell equivalent: data Greeting = Hello

greet : Greeting
greet = hello
-- ^ Haskell equivalent: greet = Hello

-- Ctrl+c Ctrl+l: load file
-- Ctrl+c Ctrl+d: deduce type
-- Ctrl+c Ctrl+n: normalise expression

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero    + y = y
(suc x) + y = suc (x + y)

_*_ : Nat → Nat → Nat
zero * y = zero
suc x * y = (x * y) + y

infixr 10 _+_
infixr 20 _*_

-- Ctrl+c Ctrl+c: case split
-- Ctrl+c Ctrl-,: information on a hole
-- Ctrl+c Ctrl-space: give solution to the hole

maximum : Nat → Nat → Nat
maximum zero y = y
maximum (suc x) zero = suc x
maximum (suc x) (suc y) = suc (maximum x y)

-- \le ≤
_≤_ : Nat → Nat → Bool
zero  ≤ y     = true
suc x ≤ zero  = false
suc x ≤ suc y = x ≤ y

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A

infixr 5 _::_

myList : List Nat
myList = 1 :: 2 :: 3 :: []

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y