
import Prelude hiding (concat)

concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss , x <- xs ]

sorted :: Ord a => [a] -> Bool
sorted xs = and bools
    where
        pairs = zip xs (drop 1 xs)
        bools = [ fst tuple <= snd tuple | tuple <- pairs ]
