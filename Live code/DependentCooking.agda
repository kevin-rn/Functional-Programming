open import Agda.Builtin.Nat
open import Agda.Builtin.List renaming (_∷_ to _::_)

data Flavour : Set where
  cheesy      : Flavour
  chocolatey  : Flavour

data Food : Flavour → Set where
  pizza   : Food cheesy
  brownie : Food chocolatey
  bread   : (f : Flavour) → Food f

amountOfCheese : Food cheesy → Nat
amountOfCheese pizza = 200
amountOfCheese (bread .cheesy) = 30

data Ingredient : Flavour → Set where
  cheese    : Ingredient cheesy
  chocolate : Ingredient chocolatey
  tomato    : Ingredient cheesy
  flour     : {f : Flavour} → Ingredient f
  eggs      : {f : Flavour} → Ingredient f

shoppingList : {f : Flavour} → Food f → List (Ingredient f)
shoppingList pizza = cheese :: flour :: tomato :: []
shoppingList brownie = flour :: eggs :: chocolate :: []
shoppingList (bread cheesy) = flour :: cheese :: []
shoppingList (bread chocolatey) = flour :: chocolate :: []
