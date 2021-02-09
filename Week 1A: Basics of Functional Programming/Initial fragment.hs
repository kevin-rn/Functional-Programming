{-
Implement the function init that removes the last element from a non-empty list, either in terms of other library functions or directly.

Hint: Some of the following library functions may come in handy:
- head :: [a] -> a
- tail :: [a] -> [a]
- length :: [a] -> Int
- reverse :: [a] -> [a]
- take :: Int -> [a] -> [a]
- drop :: Int -> [a] -> [a]
- mod :: Int -> Int -> Int
-}

init xs = undefined

-- test
prop_init :: Property
prop_init = init [1,2] === [1]


-- solution
init xs = take ((length xs) - 1) xs
