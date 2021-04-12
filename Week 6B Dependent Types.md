###  Going down fast
Implement the function downFrom : (n : Nat) → Vec Nat n that produces the vector (n-1) :: (n-2) :: ... :: 0.

### Test:
```haskell
open import Agda.Builtin.Equality

test-downFrom-type : (n : Nat) → Vec Nat n
test-downFrom-type = downFrom

test-downFrom-three : {A : Set} → downFrom 3 ≡ (2 :: 1 :: 0 :: [])
test-downFrom-three = refl

test-downFrom-zero : {A : Set} → downFrom zero ≡ []
test-downFrom-zero = refl

test-downFrom-suc : {A : Set} {n : Nat} → downFrom (suc n) ≡ n :: downFrom n
test-downFrom-suc = refl
```


##### Solution:
```haskell
open import Agda.Builtin.Nat public

data Vec (A : Set) : Nat → Set where
  []    : Vec A zero
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)
infixr 5 _::_

downFrom : (n : Nat) → Vec Nat n
downFrom zero = []
downFrom (suc n) = n :: (downFrom n)
```

### Tail risks
Implement the function tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n.

##### Library: 
```haskell
open import Agda.Builtin.Nat public

data Vec (A : Set) : Nat → Set where
  []    : Vec A zero
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)
infixr 5 _::_
```

### Test:
```haskell
open import library

tail : {A : Set}{n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs
```

##### Solution:
```haskell
open import library

tail : {A : Set}{n : Nat} → Vec A (suc n) → Vec A n
tail (x :: xs) = xs
```
__________________________________________________________________________________________________________________________________________________
### Putting the dots on the vector
Implement the function dotProduct : {n : Nat} → Vec Nat n → Vec Nat n → Nat that calculates the “dot product” (or scalar product) of two vectors. 
Note that the type of the function enforces the two vectors to have the same length, so you don’t need to write the clauses where that is not the case.  

##### Library:
```haskell
open import Agda.Builtin.Nat public

data Vec (A : Set) : Nat → Set where
  []    : Vec A zero
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)
infixr 5 _::_
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-dotProduct-type : {n : Nat} → Vec Nat n → Vec Nat n -> Nat
test-dotProduct-type = dotProduct

test-dotProduct-single : {A : Set}{x y : Nat} → dotProduct (x :: []) (y :: []) ≡ x * y + 0
test-dotProduct-single = refl

test-dotProduct-empty : {A : Set}{x : A} → dotProduct [] [] ≡ 0
test-dotProduct-empty = refl

test-dotProduct-cons : {n : Nat}{x y : Nat}{xs ys : Vec Nat n} → dotProduct (x :: xs) (y :: ys) ≡ x * y + dotProduct xs ys
test-dotProduct-cons = refl
```

##### Solution:
```haskell
-- Nat already handles * between Nat so just multiply the heads and add the recursive tail.
dotProduct : {n : Nat} → Vec Nat n → Vec Nat n → Nat
dotProduct (x :: xs) (y :: ys) = (x * y) + dotProduct xs ys
dotProduct _ _ = zero
```
__________________________________________________________________________________________________________________________________________________
### Vector update
Implement the function putVec : {A : Set}{n : Nat} → Fin n → A → Vec A n → Vec A n that sets the value at the given position in the vector to the given value, and leaves the rest of the vector unchanged.  

##### Library:
```haskell
open import Agda.Builtin.Nat public

data Vec (A : Set) : Nat → Set where
  []    : Vec A zero
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)
infixr 5 _::_

data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} → Fin n → Fin (suc n)
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-putVec-type : {A : Set}{n : Nat} → Fin n -> A -> Vec A n → Vec A n
test-putVec-type = putVec

test-putVec-single : {A : Set}{x y : A} → putVec zero x (y :: []) ≡ (x :: [])
test-putVec-single = refl

test-putVec-here : {A : Set}{n : Nat}{x y : A}{ys : Vec A n} → putVec zero x (y :: ys) ≡ (x :: ys)
test-putVec-here = refl

test-putVec-there : {A : Set}{n : Nat}{i : Fin n}{x y : A}{ys : Vec A n} → putVec (suc i) x (y :: ys) ≡ (y :: putVec i x ys)
test-putVec-there = refl
```

##### Solution:
```haskell
putVec : {A : Set}{n : Nat} → Fin n → A → Vec A n → Vec A n
putVec zero n (x :: xs) = n :: xs
putVec (suc i) n (x :: xs) = x :: putVec i n xs
```
__________________________________________________________________________________________________________________________________________________
### Seeing double
In the Library code, there are two possible implementations of the (non-dependent) pair type in Agda: one direct one as a datatype, and one type alias for the dependent pair type where the type of the second component ignores its input.   
Implement two functions from : {A B : Set} → A × B → A ×' B and to : {A B : Set} → A ×' B → A × B converting between the two representations.

##### Library: 
```haskell
data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B
  
fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

fstΣ : {A : Set}{B : A → Set} → Σ A B → A
fstΣ (x , y) = x

sndΣ : {A : Set}{B : A → Set} → (z : Σ A B) → B (fstΣ z)
sndΣ (x , y) = y

_×'_ : (A B : Set) → Set
A ×' B = Σ A (λ _ → B)
```

### Test:
```haskell
open import library

from : {A B : Set} → A × B → A ×' B
from (x , y) = x , y

to : {A B : Set} → A ×' B → A × B
to (x , y) = x , y
```

##### Solution:
```haskell
_×'_ : (A B : Set) → Set
A ×' B = Σ A (λ _ → B)

from : {A B : Set} → A × B → A ×' B
from p = ((fst p) , (snd p))

to : {A B : Set} → A ×' B → A × B
to p = ((fstΣ p) , (sndΣ p))
```
__________________________________________________________________________________________________________________________________________________
### There's lists and there's lists 
In the Library code, there are two possible implementations of the regular list type in Agda: one direct definition as a datatype, and one type alias for a dependent pair of a natural number n and a vector of length n. Implement two functions from : {A : Set} → List A → List' A and to : {A : Set} → List' A → List A converting between the two representations.

Hint. For the function from, first implement functions []' : {A : Set} → List' A and _::'_ : {A : Set} → A → List' A → List' A.  

##### Library: 
```haskell
open import Agda.Builtin.Nat public

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A
infixr 5 _::_

data Vec (A : Set) : Nat → Set where
  [] : Vec A 0
  _::_ : {n : Nat} → A → Vec A n → Vec A (suc n)

data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B
infixr 4 _,_

fstΣ : {A : Set}{B : A → Set} → Σ A B → A
fstΣ (x , y) = x

sndΣ : {A : Set}{B : A → Set} → (z : Σ A B) → B (fstΣ z)
sndΣ (x , y) = y

List' : Set → Set
List' A = Σ Nat (λ n → Vec A n)
```
### Test:
```haskell
open import Agda.Builtin.Equality

test-from-type : {A : Set} → List A → List' A
test-from-type = from

test-to-type : {A : Set} → List' A → List A
test-to-type = to

test-from-nil : {A : Set} → from {A} [] ≡ (0 , [])
test-from-nil = refl

test-from-single : {A : Set} {x : A} → from (x :: []) ≡ (1 , (x :: []))
test-from-single = refl

test-from-double : {A : Set} {x1 x2 : A} → from (x1 :: x2 :: []) ≡ (2 , x1 :: x2 :: [])
test-from-double = refl

test-from-triple : {A : Set} {x1 x2 x3 : A} → from (x1 :: x2 :: x3 :: []) ≡ (3 , x1 :: x2 :: x3 :: [])
test-from-triple = refl

test-to-nil : {A : Set} → to {A} (0 , []) ≡ []
test-to-nil = refl

test-to-single : {A : Set} {x : A} → to (1 , (x :: [])) ≡ (x :: [])
test-to-single = refl

test-to-cons : {A : Set} {x : A} {n : Nat} {xs : Vec A n} → to (suc n , (x :: xs)) ≡ x :: to (n , xs)
test-to-cons = refl

```

##### Solution:
```haskell
open import library

[]' : {A : Set} → List' A
[]' = 0 , []

_::'_ : {A : Set} → A → List' A → List' A
x ::' (n , xs) = suc n , x :: xs

from : {A : Set} → List A → List' A
from [] = []'
from (x :: xs) = x ::' from xs 

to : {A : Set} → List' A → List A
to (zero  , []       ) = []
to (suc n , (x :: xs)) = x :: to (n , xs)
```

