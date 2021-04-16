
open import Agda.Builtin.Bool
open import Agda.Builtin.Nat

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

fst : {A B : Set} → A × B → A
fst (x , y) = x

snd : {A B : Set} → A × B → B
snd (x , y) = y


data Vec (A : Set) : Nat → Set where
  []    : Vec A 0
  _::_  : {n : Nat} → A → Vec A n → Vec A (suc n)


data Σ (A : Set) (B : A → Set) : Set where
  _,_ : (x : A) → B x → Σ A B

example : Σ Nat (λ n → Vec Nat n)
example = 3 , (0 :: (1 :: (2 :: [])))


data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

cases : {A B C : Set} → 
  Either A B → (A → C) → (B → C) → C
cases (left x)  f g = f x
cases (right x) f g = g x

data ⊤ : Set where
  tt : ⊤

data ⊥ : Set where

absurd : {A : Set} → ⊥ → A
absurd ()

-- A proof that A implies A
id : {A : Set} → A → A
id x = x

example₁ : {A B : Set} → A × (A → B) → B
example₁ (x , f) = f x

example₂ : {A B C : Set} → ((A → B) × (B → C)) → A → C
example₂ (f , g) = λ x → g (f x)

example₃ : {A B : Set} → 
  ((A → ⊥) × (B → ⊥)) → (Either A B → ⊥)
example₃ (f , g) = λ x → cases x f g

not-not-lem : {A : Set} → 
  (Either A (A → ⊥) → ⊥) → ⊥
not-not-lem f = f (right {!   !})
