### Evens

Using a list comprehension, define a function that selects all the even numbers from a list.

```haskell
evens :: [Int] -> [Int]
evens xs = undefined


-- Test
prop_evens :: Property
prop_evens = evens [1,2] === [2]


-- Solution
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, x `mod` 2 == 0]

```
________________________________________________________________________________________________________________________________________________________

### Sum of squares

Using a list comprehension and the library function sum :: [Int] -> Int, define a function sum_of_squares :: Int -> Int that when given a positive integer n computes the sum 1^2 + 2^2 + 3^2 + ... + n^2 of the first n squares`.

```haskell
sum_of_squares :: Int -> Int
sum_of_squares = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Replication
Using a list comprehension, redefine the library function replicate :: Int -> a -> [a] that produces a list of identical elements. For example:
```haskell
> replicate 3 True
[True,True,True]
```

```haskell
replicate :: Int -> a -> [a]
replicate n e = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Removal
Implement a function remove :: Int -> [a] -> [a] which takes a number n and a list and removes the element at position n from the list. For example:
```haskell
> remove 1 [1,2,3,4]
[1,3,4]
```
Hint. Make use of the library functions take and drop.

```haskell



-- Test
remove :: Int -> [a] -> [a]
remove n xs = undefined


-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Pythagorean
A triple (x,y,z) of positive integers is Pytagorean if it satisfies the equation x^2 + y^2 = z^2. Using a list comprehension with three generators, define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of all such triples whose components are at most a given limit. For example:
```haskell
> pyths 10
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
```
```haskell
pyths :: Int -> [(Int,Int,Int)]
pyths n = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

###  Perfect numbers
A positive integer is perfect if it equals the sum of all its factors, excluding the number itself. Using a list comprehension and the function factors (already defined), define a function perfects :: Int -> [Int] that returns the list of all perfect numbers up to a given limit. For example:
```haskell
> perfects 500
[6,28,496]
```
```haskell
factors :: Int -> [Int]
factors n = [x | x <- [1..(n-1)], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Scalar product
The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of corresponding integers. For example, the scalar product of [1,2,3] and [4,5,6] is 1*4 + 2*5 + 3*6 = 32 Define a function scalarproduct :: [Int] -> [Int] -> Int that returns the scalar product of two lists. For example:
```haskell
> scalarproduct [1,2,3] [4,5,6]
32
```
Return 0 in case xs and ys are of different lengths.

```haskell
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct ns ms = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Riffle raffle
Implement a function riffle :: [a] -> [a] -> [a] that takes two lists of the same length and interleaves their elements in alternating order. For example:
```haskell
> riffle [1,2,3] [4,5,6]
[1,4,2,5,3,6]
```
Use a list comprehension together with the library function zip :: [a] -> [b] -> [(a,b)] to combine the two lists.

```haskell
riffle :: [a] -> [a] -> [a]
riffle ns ms = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Divisors
Implement a function divisors :: Int -> [Int] that returns the divisors of a natural number. For example:
```haskell
> divisors 15
[1,3,5,15]
```
First implement a function divides :: Int -> Int -> Bool that decides if one integer is divisible by another.

```haskell
divisors :: Int -> [Int]
divisors n = undefined


-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### What are your coordinates?
Suppose that a coordinate grid of size m×n

is given by the list of all pairs (x,y) of integers such that 0 =< x =< m and 0 =< y =< n. Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given size. For example:
```haskell
> grid 1 2
[(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
```
Next, using a list comprehension and the function grid you just defined, define a function square :: Int -> [(Int,Int)] that returns a coordinate square of size n, excluding the diagonal from (0,0) to (n,n). For example:
```haskell
> square 2
[(0,1),(0,2),(1,0),(1,2),(2,0),(2,1)]
```
```haskell
grid :: Int -> Int -> [(Int,Int)]
grid m n = undefined

square :: Int -> [(Int,Int)]
square n = undefined


-- Test



-- Solution


```
________________________________________________________________________________________________________________________________________________________

### Histogram
Write a function histogram :: [Int] -> String that takes a list of integers between 0 and 9 and outputs a vertical histogram showing the frequency of each number in the list. For example,
```haskell
> putStr (histogram [1,1,1,5])
 *        
 *        
 *   *    
==========
0123456789

> putStr [1,3,4,3,6,6,3,4,2,4,9]
   **     
   ** *   
 **** *  *
==========
0123456789
```
Note that you must use putStr to actually visualize the histogram if you are testing your code in ghci, otherwise you get a textual representation of the string such as "* *\n==========\n0123456789\n". Here on Weblab, the use of putStr is not required.

```haskell
-- Test



-- Solution


```

________________________________________________________________________________________________________________________________________________________

### Hopscotch
Implement a function skips :: [a] -> [[a]] that outputs a list of lists. The first list in the output should be the input list itself, the second list should consist of every second element of the input list, the third should consist of every third element of the input list, etc. For example:
```haskell
> skips [1,2,3,4,5,6]
[[1,2,3,4,5,6],[2,4,6],[3,6],[4],[5],[6]]
> skips [True,False]
skips [[True,False],[False]]
```
Bonus challenge. Try to find the shortest possible solution by making use of library functions such as map and foldr.

```haskell
-- Test



-- Solution


```
