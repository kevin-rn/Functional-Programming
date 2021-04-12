### Either yes or no
Under the Curry-Howard correspondence, the proposition “P or Q” corresponds to the disjoint sum type, also known as the Either type in Haskell.  
Define the Either type in Agda with constructors left and right, and implement the function cases : {A B C : Set} → Either A B → (A → C) → (B → C) → C.

### Template:
```haskell
data Either ...

cases : {A B C : Set} → Either A B → (A → C) → (B → C) → C
cases x f g = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-left-type : {A B : Set} → A → Either A B 
test-left-type = left

test-right-type : {A B : Set} → B → Either A B 
test-right-type = right

test-cases-type : {A B C : Set} → Either A B → (A → C) → (B → C) → C
test-cases-type = cases

test-cases-left : {A B C : Set} {x : A} {f : A → C} {g : B → C} → cases (left x) f g ≡ f x
test-cases-left = refl

test-cases-right : {A B C : Set} {y : B} {f : A → C} {g : B → C} → cases (right y) f g ≡ g y
test-cases-right = refl
```

### Solution:
```haskell
data Either (A B : Set) : Set where
  left   : A → Either A B
  right  : B → Either A B

cases : {A B C : Set} → Either A B → (A → C) → (B → C) → C
cases (left x)   f  g  = f x
cases (right y)  f  g  = g y
```

_______________________________________________________________________________________________________________________________________________
### Through the lens of Curry-Howard (1)

Translate the following proposition to Agda types using the Curry-Howard correspondence, and prove the statement by implementing a function of that type:
“If A then (B implies A)”

### Library: (same one for the other exercises)
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


### Template:
```haskell
open import library

prop1 : {A B : Set} → ?
prop1 = ?

```

### Test:
```haskell
open import Agda.Builtin.Equality

test-prop1-type : {A B : Set} → A → (B → A)
test-prop1-type = prop1

test-prop1 : {A B : Set} {x : A} {y : B} → prop1 x y ≡ x
test-prop1 = refl
```

### Solution:
```haskell
open import library

prop1 : {A B : Set} → A → (B → A)
prop1 = λ x y → x
```

_______________________________________________________________________________________________________________________________________________

### Through the lens of Curry-Howard (2)
Translate the following proposition to Agda types using the Curry-Howard correspondence, and prove the statement by implementing a function of that type:
“If (A and true) then (A or false)”

### Template:
```haskell
open import library

prop2 : {A : Set} → ?
prop2 = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-prop2-type : {A : Set} → (A × ⊤) → Either A ⊥
test-prop2-type = prop2

test-prop2 : {A : Set} {x : A} → prop2 (x , tt) ≡ left x
test-prop2 = refl
```

### Solution:
```haskell
open import Agda.Builtin.Equality

test-prop2-type : {A : Set} → (A × ⊤) → Either A ⊥
test-prop2-type = prop2

test-prop2 : {A : Set} {x : A} → prop2 (x , tt) ≡ left x
test-prop2 = refl
```

_______________________________________________________________________________________________________________________________________________

### Through the lens of Curry-Howard (3)
Translate the following proposition to Agda types using the Curry-Howard correspondence, and prove the statement by implementing a function of that type:
“If A implies (B implies C), then (A and B) implies C”

### Template:
```haskell
open import library

prop3 : {A B C : Set} → ?
prop3 = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-prop3-type : {A B C : Set} → (A → (B → C)) → (A × B) → C
test-prop3-type = prop3

test-prop3 : {A B : Set} {x : A} {y : B} → prop3 _,_ (x , y) ≡ (x , y)
test-prop3 = refl
```

### Solution:
```haskell
open import library

prop3 : {A B C : Set} → (A → (B → C)) → (A × B) → C
prop3 = λ f xy → f (fst xy) (snd xy)

```

_______________________________________________________________________________________________________________________________________________

### Through the lens of Curry-Howard (4)
Translate the following proposition to Agda types using the Curry-Howard correspondence, and prove the statement by implementing a function of that type:
“If A and (B or C), then either (A and B) or (A and C)”

### Template:
```haskell
open import library

prop4 : {A B C : Set} → ?
prop4 = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-prop4-type : {A B C : Set} → A × (Either B C) → Either (A × B) (A × C)
test-prop4-type = prop4

test-prop4-left : {A B C : Set} {x : A} {y : B} → prop4 (x , left {B} {C} y) ≡ left (x , y)
test-prop4-left = refl

test-prop4-right : {A B C : Set} {x : A} {z : C} → prop4 (x , right {B} {C} z) ≡ right (x , z)
test-prop4-right = refl

```

### Solution:
```haskell
open import library

prop4 : {A B C : Set} → A × (Either B C) → Either (A × B) (A × C)
prop4 = λ x → cases (snd x) (λ y → left (fst x , y)) λ z → right (fst x , z)

```

_______________________________________________________________________________________________________________________________________________

### Through the lens of Curry-Howard (5)
Translate the following proposition to Agda types using the Curry-Howard correspondence, and prove the statement by implementing a function of that type:
“If A implies C and B implies D, then (A and B) implies (C and D)”

### Template:
```haskell
open import library

prop5 : {A B C D : Set} → ?
prop5 = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-prop5-type : {A B C D : Set} → (A → C) × (B → D) → A × B → C × D
test-prop5-type = prop5

test-prop5 : {A B C D : Set} {f : A → C} {g : B → D} {x : A} {y : B}
           → prop5 (f , g) (x , y) ≡ (f x , g y)
test-prop5 = refl
```

### Solution:
```haskell
open import library

prop5 : {A B C D : Set} → (A → C) × (B → D) → A × B → C × D
prop5 = λ fg xy → fst fg (fst xy) , snd fg (snd xy)

```

_______________________________________________________________________________________________________________________________________________
### That's not not true!
Since Agda uses a constructive logic, it is not possible to prove non-constructive statements such as “for all P, either P or not P” (also known as the law of the excluded middle).   
However, we can prove the double negation of this statement: it is not not the case that for all P, either P or not P.   
To show that this double negation translation indeed works, prove this statement in Agda by implementing a function of type (Either P (P → ⊥) → ⊥) → ⊥.

### Template:
```haskell
open import library

f : {P : Set} → (Either P (P → ⊥) → ⊥) → ⊥
f h = ?
```

### Test:
```haskell
open import Agda.Builtin.Equality

test-f-type : {P : Set} → (Either P (P → ⊥) → ⊥) → ⊥
test-f-type h = f h
```

### Solution:
```haskell
open import library

f : {P : Set} → (Either P (P → ⊥) → ⊥) → ⊥
f h = h (right (λ x → h (left x)))
```

