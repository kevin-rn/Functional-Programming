### Element the third
Define functions third1 third2 third3 :: [a] -> a that all return the third element in a list that contains at least this many elements.
- third1 should be defined in terms of head and tail
- third2 should be defined using !!
- third3 should be defined using pattern matching


```haskell
third1 xs = undefined
third2 xs = undefined
third3 xs = undefined

-- Test
prop_third1 :: Property
prop_third1 = third1 [1,2,3] === 3

-- Solution
prop_third1 :: Property
prop_third1 = third1 [1,2,3] === 3
```

___________________________________________________________________________________________________________________________________________________________

### Product
Implement a function product that produces the product of a list of numbers. For example, product [2,3,4] = 24.

```haskell
-- Solution
product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * (product xs)
```

___________________________________________________________________________________________________________________________________________________________

### Reverse that list!
Implement the function reverse that reverses the elements of a list.
```haskell
-- Solution
reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = (reverse xs) ++ [x]

```

___________________________________________________________________________________________________________________________________________________________

### Gotta sum 'em all
Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value down to zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.
```haskell
-- Solution
sumdown :: Int -> Int
sumdown n | n > 0 = n + sumdown (n-1)
          | n == 0 = 0
```

___________________________________________________________________________________________________________________________________________________________

### Standard functions
Redefine the following functions from the Prelude using recursion:

- The exponentiation operator (^) :: Int -> Int -> Int on non-negative integers
- The function and :: [Bool] -> Bool deciding if all logical values in a list are True
- The function concat :: [[a]] -> [a] concatenating a list of lists.
- The function replicate :: Int -> a -> [a] producing a list with n identical elements
- The function (!!) :: [a] -> Int -> a selecting the nth element of a list
- The function elem :: Eq a => a -> [a] -> Bool deciding if a value is an element of the list.
- The function sum :: [Int] -> Int calculating the sum of a list of numbers.
- The function take :: Int -> [a] -> [a] taking a given number of elements from the start of a list.
- The function last :: [a] -> a selecting the last element of a non-empty list.

```haskell
-- Solution
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n-1))

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && (and xs)

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate x n = n : replicate (x-1) n

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(_:xs) !! n = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem y [] = False
elem y (x:xs) | x == y = True
             | otherwise = elem y xs
     
sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

take :: Int -> [a] -> [a]
take 0 xs = []
take n (x:xs) = x : (take (n-1) xs)

last :: [a] -> a
last (x:[]) = x
last (x:xs) = last xs
```

___________________________________________________________________________________________________________________________________________________________

### Power to the People
For this assignment, you will implement a more efficient version of the power function we have seen in class. 
Your implementation should make use of the fact that, if k is an even number, we can calculate n^k as follows:

n^k=(n^2)k/2=(n⋅n)^k/2  (k even)

So, instead of recursively using the case for k−1
we use the (much smaller) case for k/2. If k is not even, we simply go down one step to arrive at an even k:

n^k=n⋅n^k−1  (k odd)

Modify the definition of power to make use of this more efficient process.
Hint. Haskell has built-in functions even and odd to check whether a number is even or odd. To divide integer numbers, use the div function (and not the function (/), which is used to divide floating point and rational numbers).

```haskell
power :: Integer -> Integer -> Integer
power n 0  = 1
power n k  = n * power n (k-1)

-- Solution
power :: Integer -> Integer -> Integer
power n 0  = 1
power n k  = if odd k then n * power n (k-1) else (n^2)^(k `div` 2)
```

___________________________________________________________________________________________________________________________________________________________

### Euclid's Algorithm
Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm for calculating the greatest common divisor of two non-negative integers: if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the same process is then repeated. For example:
```haskell
> euclid 6 27
3
```

```haskell
-- Solution
euclid :: Int -> Int -> Int
euclid x y | x == y     = x
           | x > y      = euclid (x-y) y
           | otherwise  = euclid x (y-x)
```

___________________________________________________________________________________________________________________________________________________________

### Merge sort
Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example:
```haskell
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
```
Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.

Next, define a function split :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.

Using merge and split, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.

```haskell
-- Solution
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

split :: [a] -> ([a],[a])
split xs = splitAt halve xs
           where halve = length xs `div` 2


msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort as = merge(msort start) (msort end)
           where (start, end) = split as
```

___________________________________________________________________________________________________________________________________________________________

### Bag equality
Two lists are ‘bag equal’ if they contain the same elements, but possibly in a different order. Implement a function bagEqual :: (Eq a) => [a] -> [a] -> Bool that checks if two given lists are bag equal.

Hint: you can make use of the library functions elem :: (Eq a) => a -> [a] -> Bool and delete :: (Eq a) => a -> [a] -> [a].
```haskell
-- Solution
bagEqual :: (Eq a) => [a] -> [a] -> Bool
bagEqual [] ys = ys == []
bagEqual (x:xs) ys = if (elem x ys) then (bagEqual xs (delete x ys))
                     else False

```

___________________________________________________________________________________________________________________________________________________________

### Bank card numbers
Define a function luhn :: [Int] -> Bool that implements the Luhn algorithm to check if a given bank card number is valid.

As a reminder, the Luhn algorithm (Wikipedia) is used to check bank card numbers for simple errors such as mistyping a digit, and proceeds as follows:
- consider each digit as a separate number;
- moving left, double every other number from the second last;
- subtract 9 from each number that is now greater than 9;
- add all the resulting numbers together;
- if the total is divisible by 10, the card number is valid.


```haskell
luhn :: [Int] -> Bool
luhn = undefined

-- Test
xs :: [Int]
xs = [7,9,9,2,7,3,9,8,7,1,3]

ys :: [Int]
ys = [4,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]

prop_luhn :: Property
prop_luhn = luhn xs === True

prop_luhn1 :: Property
prop_luhn1 = luhn ys === True

-- Solution
helper :: [Int] -> [Int]
helper [] = []
helper [x] = [x]
helper (x:y:z) = if double > 9 then x:(double-9):list
                     else x:double:list
                      where
                        double = y*2 
                        list = helper z

luhn :: [Int] -> Bool
luhn xs = sum (helper (reverse xs)) `mod` 10 == 0
```

___________________________________________________________________________________________________________________________________________________________

### Towers of Hanoi
The Towers of Hanoi ([wikipedia](https://en.wikipedia.org/wiki/Tower_of_Hanoi)) is a classic puzzle with a solution that can be described recursively. Disks of different sizes are stacked on three pegs; the goal is to get from a starting configuration with all disks stacked on the first peg to an ending configuration with all disks stacked on the last peg. The rules are as follows:

    Only one disk can be moved at a time
    Each move consists of taking the upper disk from one of the stacks and placing it on top of another stack or on an empty rod.
    No larger disk may be placed on top of a smaller disk.

The recursive solution to this problem can be solved as follows. If only one peg has to be moved, move it from the source peg to the target peg directly. If (m) pegs ((m > 0)) have to be moved, proceed as follows:
- Move (m-1) disks from the source peg to the spare peg, by applying this procedure recursively.
- Move the largest disk from the source peg to the target peg.
- Move (m-1) disks from the spare peg to the target peg, again applying this procedure recursively.

The goal of this exercise is to implement the function hanoi :: Int -> Peg -> Peg -> Peg -> [Move], where type Peg = String and type Move = (Peg,Peg) are type synonyms, such that hanoi n source target spare computes the list of moves for solving the puzzle with n disks. For example,
```haskell
> hanoi 2 "a" "b" "c"
[("a","c"),("a","b"),("c","b")]
```

```haskell
type Peg = String
type Move = (String, String)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n source target spare = undefined

-- Solution
type Peg = String
type Move = (String, String)

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi n source target spare = hanoilist n source target spare []
  where
    hanoilist 0 _ _ _ list = list
    hanoilist n a b c list = hanoilist (n-1) a c b ((a, b) : hanoilist (n-1) c b a list)
```


___________________________________________________________________________________________________________________________________________________________

### Local maxima
A local maximum of a list is an element of the list that is strictly greater than the elements right before and after it. For example, in the list [3,5,2,3,4], the only local maximum is 5, since it is both greater than 3 and greater than 2. 4 is not a local maximum because there is no element that comes after it.

Write a function localMaxima :: [Int] -> [Int] that computes all the local maxima in the given list and returns them in order. For example
```haskell
> localMaxima [2,9,5,6,1]
[9,6]
> localMaxima [2,3,4,1,5]
[4]
> localMaxima [1,2,3,4,5]
[]
```

```haskell
-- Solution
localMaxima :: [Int] -> [Int]
localMaxima (x:y:z:zs) = if x < y && y > z then y : localMaxima (y:z:zs)
                         else localMaxima (y:z:zs)
localMaxima _ = []

```
