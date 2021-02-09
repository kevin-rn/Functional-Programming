-{
Using library functions, define a function halve :: [a] -> ([a],[a]) that splits an even-lengthed list into two halves. For example:

> halve [1,2,3,4,5,6]
([1,2,3],[4,5,6])

Hint: Some of the following library functions may come in handy:
- head :: [a] -> a
- tail :: [a] -> [a]
- length :: [a] -> Int
- reverse :: [a] -> [a]
- take :: Int -> [a] -> [a]
- drop :: Int -> [a] -> [a]
- mod :: Int -> Int -> Int
}-

halve xs = undefined

-- test
prop_halve :: Property
prop_halve = halve [1,2] === ([1],[2])


-- solution
halve xs = splitAt (((length xs)+1) `div` 2) xs
