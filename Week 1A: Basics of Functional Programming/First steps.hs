-- Write a function called add_and_double which adds two numbers together and then doubles the result, by replacing undefined with the proper expression.
add_and_double x y = undefined

-- test 
prop_add_and_double_example :: Property
prop_add_and_double_example = add_and_double 1 1 === 4

-- solution
add_and_double x y = (x + y)*2
