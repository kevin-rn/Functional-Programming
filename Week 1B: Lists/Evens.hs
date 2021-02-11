--Using a list comprehension, define a function that selects all the even numbers from a list
evens :: [Int] -> [Int]
evens xs = undefined


-- Test
prop_evens :: Property
prop_evens = evens [1,2] === [2]


-- Solution
evens :: [Int] -> [Int]
evens xs = [x | x <- xs, x `mod` 2 == 0]
