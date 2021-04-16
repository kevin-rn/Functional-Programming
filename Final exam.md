### Question 1 Theory Question

Suppose that you have been invided to write an article for a professional computing magazine on the benefits that pure functional programming brings to programmers.   
Give a list of at least 5 benefits that you would mention, and illustrate each of the benefits with a small example written in Haskell.
(Note that this is the same question as on the practice exam, this is not a mistake.)

```
1) There are no side effects to any function, easier to contain effects of a function e.g.

x = 1
foo :: Int -> Int
foo a = 1

x = foo x will not change the value of the initial x and give an error
foo x is the way functional programming handles it by creating a new value and returning it

2) Some functions are better implemented in a functional form, eg: quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let smallerSorted = quicksort1 [a | a <- xs, a <= x]
biggerSorted = quicksort1 [a | a <- xs, a > x]
in smallerSorted ++ [x] ++ biggerSorted

3) Recursive functions are easier in the functional paradigm, e.g.
loop :: a -> [a]
loop a = a : loop a

4) It has support for lazy evaluation, allowing for infinite data structures, e.g.
head [1..] returns 1

5) It supports higher order functions, meaning its possible to pass one function onto another, e.g. map (+1) [1..5] will add 1 to every element of the list
```
________________________________________________________________________________________________________________________________________

### Question 2 Multiple choice


________________________________________________________________________________________________________________________________________

### Question 3 Defining and testing functions


________________________________________________________________________________________________________________________________________

### Question 4 Data types and type classes

________________________________________________________________________________________________________________________________________

### Question 5 Functors, Applicative, Monads

________________________________________________________________________________________________________________________________________

### Question 6 Laziness

________________________________________________________________________________________________________________________________________

### Question 7 The Curry-Howard correspondence

________________________________________________________________________________________________________________________________________

### Question 8 Equational reasoning




