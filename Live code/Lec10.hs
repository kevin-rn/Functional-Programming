
-- Two versions of the repeat function.
repeat1 :: a -> [a]
repeat1 x = xs
  where xs = x : xs

repeat2 :: a -> [a]
repeat2 x = x : repeat2 x

sieve :: [Integer] -> [Integer]
sieve (x:xs) = 
  let xs' = [ y | y <- xs, y `mod` x /= 0 ]
  in  x : sieve xs'

-- Three different implementations of the list `primes`
primesV1 :: [Integer]
primesV1 = sieve [2..]

multiples :: [[Integer]]
multiples = [ map (*n) [n..] | n <- [2..] ]

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) 
  | x < y     = x : merge xs (y:ys)
  | x == y    = x : merge xs ys
  | otherwise = y : merge (x:xs) ys

xmerge :: Ord a => [a] -> [a] -> [a]
xmerge (x:xs) ys = x:merge xs ys

mergeAll :: Ord a => [[a]] -> [a]
mergeAll (xs:xss) = merge xs (mergeAll xss)

composites :: [Integer]
composites = mergeAll multiples

{- Here is a representation of the infinite
list of infinite lists `multiples`:

... ... ... ... ... ...
12  21  32  45  60  ...
10  18  28  40  54  ...
8   15  24  35  48  ...
6   12  20  30  42  ...
4   9   16  25  36  ...

-}

(\\) :: Ord a => [a] -> [a] -> [a]
[] \\ _ = []
xs \\ [] = xs
(x:xs) \\ (y:ys)
  | x < y     = x : (xs \\ (y:ys))
  | x == y    = xs \\ ys
  | otherwise = (x:xs) \\ ys

primesV2 :: [Integer]
primesV2 = [2..] \\ composites


primesV3 :: [Integer]
primesV3 = 2 : ([3..] \\ composites)
  where
    composites = mergeAll primeMultiples
    primeMultiples = [ map (p*) [p..] | p <- primesV3  ]


-- Doubly-linked lists
data DLL a = Node { elem  :: a
                  , mPrev :: Maybe (DLL a) 
                  , mNext :: Maybe (DLL a) }

prev :: DLL a -> DLL a
prev n = case mPrev n of
  Just x  -> x
  Nothing -> error "no previous item!"

next :: DLL a -> DLL a
next n = case mNext n of
  Just x -> x
  Nothing -> error "no next item!"

instance Show a => Show (DLL a) where
  show (Node x _ _) = show x

listToDLL' :: [a] -> Maybe (DLL a) -> DLL a
listToDLL' []     prev = error "empty list"
listToDLL' [x]    prev = Node x prev Nothing
listToDLL' (x:xs) prev = 
  let this = Node x prev (Just next)
      next = listToDLL' xs (Just this)
  in this 

listToDLL :: [a] -> DLL a
listToDLL xs = listToDLL' xs Nothing

listFromDLL :: DLL a -> [a]
listFromDLL (Node x _ mnext) = 
  case mnext of
    Nothing   -> [x]
    (Just xs) -> x : listFromDLL xs


-- Labeling an infinite tree
data Tree a = Leaf | TNode (Tree a) a (Tree a)
  deriving (Show)

labelTree :: [Int] -> Tree a -> Tree (Int,a)
labelTree xs Leaf = Leaf
labelTree (x:xs) (TNode l a r) =
    let (ys,zs) = split xs
        l'      = labelTree ys l
        r'      = labelTree zs r
    in  TNode l' (x,a) r'

  where
      split :: [Int] -> ([Int],[Int])
      split [] = error "Expected infinite list!"
      split (x:y:xs) = 
          let (ys,zs) = split xs
          in  (x:ys , y:zs)
