-- Define a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the empty list to itself rather than producing an error.

safetail xs = undefined

-- test 
prop_safetail :: Property
prop_safetail = safetail [1,2] === [2]

-- solution
safetail xs = if xs == [] then [] else tail xs

-- Or

safetail xs 
  | [] == xs = []
  | otherwise = tail xs
