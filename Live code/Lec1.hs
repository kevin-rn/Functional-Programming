import Prelude hiding (sum)

sum :: Num p => [p] -> p
sum []     = 0
sum (x:xs) = x + sum xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [ y | y <- xs, y <  x ]
        larger  = [ y | y <- xs, y >= x ]

seqn :: Monad m => [m ()] -> m ()
seqn []     = return ()
seqn (x:xs) =
    do
        x
        seqn xs

main :: IO ()
main = seqn [ print n | n <- [1..10] ]

primes :: [Int]
primes = filterPrimes [2..]
  where
      filterPrimes (x:xs) = x : filterPrimes [ y | y <- xs , y `mod` x /= 0 ]














{-
seqn :: Monad m => [m ()] -> m ()
seqn [] = return ()
seqn (x:xs) = 
  do
    x
    seqn xs

main :: IO ()
main = seqn [ print n | n <- [1..10] ]

primes :: [Integer]
primes = filterPrimes [2..]
  where
    filterPrimes (x:xs) = 
      x : filterPrimes [ y | y <- xs, y `mod` x /= 0 ]
      -}