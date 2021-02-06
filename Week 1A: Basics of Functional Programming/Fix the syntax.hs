-- The given script contains three syntactic errors. Correct these errors and check that the value of n is computed correctly.
n =  A `div` length xs
     where
         A = 10
        xs = [1,2,3,4,5]

-- test
prop_n :: Property
prop_n = n === 2

-- solution
n =  a `div` length xs
     where
        a = 10
        xs = [1,2,3,4,5]
