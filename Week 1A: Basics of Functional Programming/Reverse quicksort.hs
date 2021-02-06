-- Modify the definition of the function qsort so that is produces a reverse sorted version of the list.
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
