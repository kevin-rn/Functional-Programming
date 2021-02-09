{- 

Write a function called quadratic_solutions that takes in three arguments of type Double (a, b, and c) and returns a list consisting of all (real-valued) solutions 
of the quadratic equation ax2+bx+c=0. 
Hint. Use a let or where expression to define the square root of the discriminant √(b^2−4ac).

-}

quadratic_solutions :: Double -> Double -> Double -> [Double]
quadratic_solutions = undefined


-- test
prop_quadradic_solutions :: Property
prop_quadradic_solutions = quadratic_solutions 1 0 (-9) === [3,-3]

-- solution
quadratic_solutions :: Double -> Double -> Double -> [Double]
quadratic_solutions a b c  = if d < 0 then [] else [x]++[y]
                              where
                                x = (-b + sqrt d) / (2 * a)
                                y = (-b - sqrt d) / (2 * a)
                                d = b ^ 2 - 4 * a * c
                                
