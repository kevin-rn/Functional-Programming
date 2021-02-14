```haskell
import Data.List
import Data.Function

reindex :: (Int -> Int) -> [a] -> [a]
reindex f xs = map snd (sortBy (compare `on` fst) newList) 
  where
    indices = map f [0..]
    xss (x,_) = x >= 0 && x < (length xs)
    newList = filter xss (zip indices xs)
 ```
