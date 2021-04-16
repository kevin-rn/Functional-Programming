open import Agda.Builtin.Bool
open import Agda.Builtin.Nat
open import Agda.Builtin.List renaming (_∷_ to _::_)

id : {A : Set} → A → A
id x = x

map : {A B : Set} → (A → B) → List A → List B
map f []         = []
map f (x :: xs)  = f x :: map f xs



data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x
infix 4 _≡_

sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

trans : {A : Set} {x y z : A} →
  x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

cong : {A B : Set} {x y : A} →
  (f : A → B) → x ≡ y → f x ≡ f y
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

add-assoc : (x y z : Nat) → x + (y + z) ≡ (x + y) + z
add-assoc zero    y z = 
  begin
    zero + (y + z)
  =⟨⟩
    y + z  
  =⟨⟩ 
    (zero + y) + z
  end
add-assoc (suc x) y z =
  begin
    suc x + (y + z)
  =⟨⟩
    suc (x + (y + z))
  =⟨ cong suc (add-assoc x y z) ⟩
    suc ((x + y) + z)
  =⟨⟩
    suc (x + y) + z
  =⟨⟩
    (suc x + y) + z
  end


_∘_ : {A B C : Set} → (B → C) → (A → B) → (A → C)
f ∘ g = λ x → f (g x)

list-functor-comp : {A B C : Set} →
  (f : B → C) (g : A → B) (xs : List A) →
  map (f ∘ g) xs ≡ (map f ∘ map g) xs
list-functor-comp f g [] = 
  begin
    map (f ∘ g) []
  =⟨⟩
    []
  =⟨⟩
    map f []
  =⟨⟩
    map f (map g [])
  =⟨⟩
    (map f ∘ map g) []
  end
list-functor-comp f g (x :: xs) = 
  begin
    map (f ∘ g) (x :: xs)
  =⟨⟩
    (f ∘ g) x :: map (f ∘ g) xs
  =⟨⟩
    f (g x) :: map (f ∘ g) xs
  =⟨ cong (f (g x) ::_) (list-functor-comp f g xs) ⟩
    f (g x) :: (map f ∘ map g) xs
  =⟨⟩
    f (g x) :: map f (map g xs)
  =⟨⟩ 
    map f (g x :: map g xs)
  =⟨⟩
    map f (map g (x :: xs))
  =⟨⟩
    (map f ∘ map g) (x :: xs)
  end
