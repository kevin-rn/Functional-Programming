
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

data Bool : Set where
  true  : Bool
  false : Bool

not : Bool → Bool
not true  = false
not false = true

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y

data Vec (A : Set) : Nat → Set where
  []    : Vec A 0
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)

infixr 5 _::_

tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs

zipVec : {A B : Set} {n : Nat} → Vec A n → Vec B n → Vec (A × B) n
zipVec [] [] = []
zipVec (x :: xs) (y :: ys) = (x , y) :: zipVec xs ys 

testZipVec = zipVec (1 :: 2 :: 3 :: []) (4 :: 5 :: 6 :: [])

data Fin : Nat → Set where
  zero  : {n : Nat} → Fin (suc n)
  suc   : {n : Nat} → Fin n → Fin (suc n)

noFinZero : {A : Set} → Fin 0 → A
noFinZero ()

lookupVec : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookupVec (x :: xs) zero    = x
lookupVec (x :: xs) (suc i) = lookupVec xs i