### First steps
Write a function called add_and_double which adds two numbers together and then doubles the result, by replacing undefined with the proper expression.

```haskell
add_and_double x y = undefined

-- Test
prop_add_and_double_example :: Property
prop_add_and_double_example = add_and_double 1 1 === 4

-- Solution
add_and_double x y = (x + y)*2

```

_______________________________________________________________________________________________________________________________________________________

### Fix the syntax
The given script contains three syntactic errors. Correct these errors and check that the value of n is computed correctly.
```haskell
n =  A `div` length xs
     where
         A = 10
        xs = [1,2,3,4,5]
        
-- Test
prop_n :: Property
prop_n = n === 2

-- Solution
n =  a `div` length xs
     where
        a = 10
        xs = [1,2,3,4,5]

```

_______________________________________________________________________________________________________________________________________________________

### Reverse quicksort
Modify the definition of the function qsort so that is produces a reverse sorted version of the list.

```haskell
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
    where
      smaller = [ y | y <- xs , y <= x ]
      larger  = [ y | y <- xs , y > x  ]

-- Test
prop_qsort :: Property
prop_qsort = qsort [1,2] === [2,1]

-- Solution
qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) =
  qsort larger ++ [x] ++ qsort smaller
    where
      smaller = [ y | y <- xs , y <= x ]
      larger  = [ y | y <- xs , y > x  ]

```

_______________________________________________________________________________________________________________________________________________________
###     Any definition will do

Write down definitions that have the following types; it does not matter what the definitions actually do as long as their types are correct.
```haskell
bools :: [Bool]
nums :: [[Int]]
add :: Int -> Int -> Int -> Int
copy :: a -> (a,a)
```

```haskell
-- Solution
-- bools :: [Bool]
bools = [True]

-- nums :: [[Int]]
nums = [[1]]

-- add :: Int -> Int -> Int -> Int
add a b c = a + b + c

-- copy :: a -> (a,a)
copy a = (a, a)

```

_______________________________________________________________________________________________________________________________________________________
### Half the list it used to be
Using library functions, define a function halve :: [a] -> ([a],[a]) that splits an even-lengthed list into two halves. For example:
```haskell
> halve [1,2,3,4,5,6]
([1,2,3],[4,5,6])
```
Hint: Some of the following library functions may come in handy:
- head :: [a] -> a
- tail :: [a] -> [a]
- length :: [a] -> Int
- reverse :: [a] -> [a]
- take :: Int -> [a] -> [a]
- drop :: Int -> [a] -> [a]
- mod :: Int -> Int -> Int

```haskell
halve xs = undefined

-- Test
prop_halve :: Property
prop_halve = halve [1,2] === ([1],[2])

-- Solution
halve xs = splitAt (((length xs)+1) `div` 2) xs

```

_______________________________________________________________________________________________________________________________________________________
### Initial fragment
Implement the function init that removes the last element from a non-empty list, either in terms of other library functions or directly.

Hint: Some of the following library functions may come in handy:
- head :: [a] -> a
- tail :: [a] -> [a]
- length :: [a] -> Int
- reverse :: [a] -> [a]
- take :: Int -> [a] -> [a]
- drop :: Int -> [a] -> [a]
- mod :: Int -> Int -> Int

```haskell
init xs = undefined

-- Test
prop_init :: Property
prop_init = init [1,2] === [1]

-- Solution
init xs = take ((length xs) - 1) xs

```

_______________________________________________________________________________________________________________________________________________________
### Tail, but safer
Define a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the empty list to itself rather than producing an error.

```haskell
safetail xs = undefined

-- Test
prop_safetail :: Property
prop_safetail = safetail [1,2] === [2]

-- Solution
safetail xs 
  | [] == xs = []
  | otherwise = tail xs


safetail xs = if xs == [] then [] else tail xs  --alternative
```

_______________________________________________________________________________________________________________________________________________________
### Quadratic equations
Write a function called quadratic_solutions that takes in three arguments of type Double (a, b, and c) and returns a list consisting of all (real-valued) solutions of the quadratic equation ax2+bx+c=0. 
Hint. Use a let or where expression to define the square root of the discriminant √(b^2−4ac).

```haskell
quadratic_solutions :: Double -> Double -> Double -> [Double]
quadratic_solutions = undefined
-- Test
prop_quadradic_solutions :: Property
prop_quadradic_solutions = quadratic_solutions 1 0 (-9) === [3,-3]

-- Solution
quadratic_solutions :: Double -> Double -> Double -> [Double]
quadratic_solutions a b c  = if d < 0 then [] else [x]++[y]
                              where
                                x = (-b + sqrt d) / (2 * a)
                                y = (-b - sqrt d) / (2 * a)
                                d = b ^ 2 - 4 * a * c

```

_______________________________________________________________________________________________________________________________________________________
### Luhn Algorithm
The Luhn algorithm ([Wikipedia](https://en.wikipedia.org/wiki/Luhn_algorithm)) is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:

- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.

Define a function luhnDouble :: Int -> Int that doubles a digit and subtracts 9 if the result is greater than 9. For example:
```haskell
> luhnDouble 3
6

> luhnDouble 6
3
```
Using luhnDouble and the integer remainder function mod, define a function luhn :: Int -> Int -> Int -> Int -> Bool that decides if a four-digit bank card number is valid. For example:
```haskell
> luhn 1 7 8 4
True

> luhn 4 7 8 3
False
```
Now define a function luhnFinal :: Int -> Int -> Int -> Bool that returns the fourth digit of a four-digit bank card number. For example:
```haskell
> luhnFinal 1 7 8
4

> luhnFinal 4 7 8
8
```
```haskell
luhnDouble :: Int -> Int
luhnDouble = undefined

luhn :: Int -> Int -> Int -> Int -> Bool
luhn = undefined

luhnFinal :: Int -> Int -> Int -> Int
luhnFinal = undefined

-- Test
prop_luhnDouble :: Property
prop_luhnDouble = luhnDouble 3 === 6

prop_luhn :: Property
prop_luhn = (luhn 1 7 8 4) === True

prop_luhnFinal :: Property
prop_luhnFinal = (luhnFinal 1 7 8) === 4

-- Solution
-- Doubles the current number and substracts 9 if result is greater than 9
luhnDouble :: Int -> Int
luhnDouble x = let y = (2 * x) in if y > 9
    then y - 9
    else y

-- Checks if 4 digits form valid bank card number
luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = sum[luhnDouble a, b, luhnDouble c, d] `mod` 10 == 0

-- Calculates final luhn digit
luhnFinal :: Int -> Int -> Int -> Int
luhnFinal a b c = 10 - rest
                  where
                    rest = sum[luhnDouble a, b, luhnDouble c] `mod` 10

```




```
