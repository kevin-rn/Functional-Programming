### Replication replication replication ... 
Consider the following function:

    replicate : {A : Set} → Nat → A → List A
    replicate zero    x = []
    replicate (suc n) x = x :: replicate n x

Complete the proof that the length of replicate n x is always equal to n, by induction on the number n.

##### Template:
```haskell
open import library

length-replicate : {A : Set} → (n : Nat) (x : A) → length (replicate n x) ≡ n

length-replicate {A} zero x =
  begin
    length (replicate zero x)
  =⟨⟩                          -- applying replicate
    length {A} []
  =⟨⟩                          -- applying length
    zero
  end

length-replicate (suc n) x =
  begin
    length (replicate (suc n) x)
  =⟨⟩
    ?
  =⟨⟩
    ?
  =⟨ ? ⟩
    suc n
  end


```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A

length : {A : Set} → List A → Nat
length []         = 0
length (x :: xs)  = suc (length xs)

replicate : {A : Set} → Nat → A → List A
replicate zero    x = []
replicate (suc n) x = x :: replicate n x
```

##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

### Reasoning about addition
In the lecture notes, it is proven that n + zero equals n for all natural numbers n. Following the example of this proof, now write down a proof that m + (suc n) is equal to suc (m + n) for all natural numbers m and n.
Bonus question. Now write down a proof of commutativity of addition: m + n equals n + m for all natural numbers m and n, by making use of the previous two lemmas.

##### Template:
```haskell
open import library

add-n-zero : (n : Nat) → n + zero ≡ n
add-n-zero zero =
  begin
    zero + zero
  =⟨⟩                             -- applying +
    zero
  end
add-n-zero (suc n) =
  begin
    (suc n) + zero
  =⟨⟩                             -- applying +
    suc (n + zero)
  =⟨ cong suc (add-n-zero n) ⟩    -- using induction hypothesis
    suc n
  end

add-n-suc : (m n : Nat) → m + (suc n) ≡ suc (m + n)
add-n-suc m n = ?

-- Bonus part: prove commutativity of addition.
add-comm : (m n : Nat) → m + n ≡ n + m
add-comm m n = ?

```


### Library: 
```haskell
```

##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

### 

##### Template:
```haskell

```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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


##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

###  Length of map
Prove that using map does not change the length of a list, i.e. that length (map f xs) is equal to length xs.

##### Template:
```haskell
open import library

length-map : {A B : Set} (f : A → B) (xs : List A) → length (map f xs) ≡ length xs
length-map {A} {B} f xs = ?

```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A

length : {A : Set} → List A → Nat
length []         = 0
length (x :: xs)  = suc (length xs)

_++_ : {A : Set} → List A → List A → List A
[]         ++  ys  = ys
(x :: xs)  ++  ys  = x :: (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map f []         = []
map f (x :: xs)  = f x :: map f xs
```


##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

###  Append nothing
Prove that xs ++ [] is equal to xs (see Library code for the definition of _++_).

##### Template:
```haskell
open import library

append-[] : {A : Set} → (xs : List A) → xs ++ [] ≡ xs
append-[] xs = ?

```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A

_++_ : {A : Set} → List A → List A → List A
[]         ++  ys  = ys
(x :: xs)  ++  ys  = x :: (xs ++ ys)
```


##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

### Append more
Prove that (xs ++ ys) ++ zs is equal to xs ++ (ys ++ zs) (see Library code for the definition of _++_).

##### Template:
```haskell
open import library

append-assoc : {A : Set} → (xs ys zs : List A)
             → (xs ++ ys) ++ zs ≡ xs ++ (ys ++ zs)
append-assoc xs ys zs = ?

```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A

_++_ : {A : Set} → List A → List A → List A
[]         ++  ys  = ys
(x :: xs)  ++  ys  = x :: (xs ++ ys)
```


##### Test:
```haskell

```

##### Solution:
```haskell

```

__________________________________________________________________________________________________________________________________________________

### Take it or leave it 
Define the functions take and drop that respectively return or remove the first n elements of the list (or all elements if the list is shorter).
Next, prove that for any number n and any list xs, we have take n xs ++ drop n xs = xs.

##### Template:
```haskell
open import library

take : {A : Set} → Nat → List A → List A
take n xs = ?

drop : {A : Set} → Nat → List A → List A
drop n xs = ?

take-drop : {A : Set} (n : Nat) (xs : List A) → take n xs ++ drop n xs ≡ xs
take-drop n xs = ?

```

### Library: 
```haskell
open import Agda.Builtin.Nat public
open import Agda.Builtin.Equality public

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

data List (A : Set) : Set where
  []    : List A
  _::_  : A → List A → List A

infixr 10 _::_

length : {A : Set} → List A → Nat
length []         = 0
length (x :: xs)  = suc (length xs)

_++_ : {A : Set} → List A → List A → List A
[]         ++  ys  = ys
(x :: xs)  ++  ys  = x :: (xs ++ ys)

map : {A B : Set} → (A → B) → List A → List B
map f []         = []
map f (x :: xs)  = f x :: map f xs
```


##### Test:
```haskell

```

##### Solution:
```haskell

```


__________________________________________________________________________________________________________________________________________________

### 

##### Template:
```haskell

```

### Library: 
```haskell
```


##### Test:
```haskell

```

##### Solution:
```haskell

```
