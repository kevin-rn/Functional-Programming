### Testing the sum
The standard Haskell function sum :: Num a => [a] -> a is fully defined by the following properties:
- sum [] = 0
- sum [x] = x 
- sum (xs ++ ys) = sum xs + sum ys

Write three property testsprop_sum_empty, prop_sum_singleton, and prop_sum_concat to verify that these properties indeed hold.

##### Solution
```haskell
prop_sum_empty = sum [] == 0

prop_sum_singleton x = sum [x] == x

prop_sum_concat xs ys = sum xs + sum ys == sum (xs ++ ys)
```
_________________________________________________________________________________________________________________________________________________________________
### Testing the sort
Suppose we have a function sort :: Ord a => [a] -> [a]. In order to test this function, we can write a property that the output is always sorted:
```haskell
sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _        = True

prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted xs = sorted (sort xs)
```
However, this is not enough to fully specify the sort function: a trivial definition such as sort xs = [] also satisfies it. 
We also need to test that the input and output have the same elements.

Implement a function sameElements :: Eq a => [a] -> [a] -> Bool that returns True if the two given lists have precisely the same elements (but possibly in a different order). Then write a test prop_sort_sameElements to test that the input and output of the sort function always have the same elements.

##### Solution
```haskell
sorted :: Ord a => [a] -> Bool
sorted (x:y:ys) = x <= y && sorted (y:ys)
sorted _        = True

prop_sort_sorted :: [Int] -> Bool
prop_sort_sorted xs = sorted (sort xs)

-- Same as bagEquality function from week 2A
sameElements :: Eq a => [a] -> [a] -> Bool
sameElements [] ys = ys == []
sameElements (x:xs) ys = if (elem x ys) then (sameElements xs (delete x ys))
                     else False

prop_sort_sameElements xs = sameElements xs (sort xs)
```
_________________________________________________________________________________________________________________________________________________________________
### Testing the index
There are several ways to get the nth element of a list in Haskell. 
There is of course the builtin function (!!), but we can for example also use drop to remove the first n elements and then take the head of the list. 
We can try to test that the two methods are equivalent as follows:
```haskell
prop_index :: [Int] -> Int -> Bool
prop_index xs n = xs !! n == head (drop n xs)
```
However, this results in an error:
```haskell
Property prop_index failed!
*** Failed! Exception: 'Prelude.!!: index too large' (after 1 test):
[]
0
```
Fix the test so that it tests the correct property.

##### Solution
```haskell
prop_index :: [Int] -> Int -> Property
prop_index xs n = (n > 0 && n < length xs) ==> xs !! n == head (drop n xs)
```
_________________________________________________________________________________________________________________________________________________________________
### Testing the halve
In one of the first exercises, you implemented a function halve :: [a] -> ([a],[a]) that splits an even-lengthed list into two halves. 
Now write a QuickCheck property prop_halve_sameLength to test the following property: if the input is a list of even length, then the two halves have the same length.

##### Solution
```haskell
prop_halve_sameLength xs = even_list xs ==> length as == length bs
        where 
          (as, bs) = halve xs
          even_list xs = length xs `mod` 2 == 0
```
