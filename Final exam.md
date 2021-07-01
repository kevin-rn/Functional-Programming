### Question 1 Theory Question

Suppose that you have been invided to write an article for a professional computing magazine on the benefits that pure functional programming brings to programmers.   
Give a list of at least 5 benefits that you would mention, and illustrate each of the benefits with a small example written in Haskell.
(Note that this is the same question as on the practice exam, this is not a mistake.)

```haskell
1) There are no side effects to any function, easier to contain effects of a function e.g.

x = 1
foo :: Int -> Int
foo a = 1

x = foo x will not change the value of the initial x and give an error
foo x is the way functional programming handles it by creating a new value and returning it

2) Some functions are better implemented in a functional form, eg: quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = let smallerSorted = quicksort1 [a | a <- xs, a <= x]
biggerSorted = quicksort1 [a | a <- xs, a > x]
in smallerSorted ++ [x] ++ biggerSorted

3) Recursive functions are easier in the functional paradigm, e.g.
loop :: a -> [a]
loop a = a : loop a

4) It has support for lazy evaluation, allowing for infinite data structures, e.g.
head [1..] returns 1

5) It supports higher order functions, meaning its possible to pass one function onto another, e.g. map (+1) [1..5] will add 1 to every element of the list


1 points for each valid benefit, and 1 more point if you also explain why it is useful to the programmer.

Here are some benefits (not an exhaustive list):
- Static typing leads to fewer errors at run-time.
- Type inference improves programmer productivity.
- Purity means side effects must be explicit in the type, which allows for easier reasoning and optimization.
- Automatic memory management and the lack of null pointers means a large class of common errors disappears entirely.
- Higher-order functions can be used to encapsulate common programming patterns.
- Laziness can be used to write code in a more modular way by separating data and control.
- Type classes can be used to write functions on a more abstract level and encourage code reuse.
```
________________________________________________________________________________________________________________________________________

### Question 2 Multiple choice
```haskell
Which of the following definitions of concat correctly transforms a list of lists into a single list?
[ ] concat yss = [y | y <- yss]
[ ] concat yss = concat (tail yss)
[ ] concat yss = map (++) yss
[x] concat yss = [y | ys <- yss, y <- ys]

Which of the following equations is true for all lists xs in Haskell?
[ ] [[]] ++ xs == xs
[x] [[]] ++ [xs] == [[],xs]
[ ] xs:xs == [xs,xs]
[ ] []:xs == [[],xs]

Which of the following equations does NOT hold for all functions f?
[ ] map f . reverse = reverse . map f
[x] map f . sort = sort . map f
[ ] map f . drop n = drop n . map f
[ ] map f . take n = take n . map f

Which of the following defines the infinite list pows = 1:2:4:8:16...?
[x] pows = 1 : map (*2) pows
[ ] pows = map (*2) [1..]
[ ] pows = 1 : map (*2) (tail pows)
[ ] pows = 1 : 2 : tail pows

How many elements are there in the Agda type Vec (Fin 2) 5?
[ ] 0
[ ] 1
[ ] 2
[ ] 5
[x] 10
[ ] 25
[ ] 32

```

________________________________________________________________________________________________________________________________________

### Question 3 Defining and testing functions
Given a list of values of some type a that implements the Ord type class, the local extrema are the values that are either strictly bigger or strictly smaller than the numbers immediately before or after them. The goal of this question is to implement two different versions of the function localExtrema :: Ord a => [a] -> [a] that returns the list of all local extrema in a given list. The first and last elements of a list are never considered to be local extrema.

```haskell
Examples:
localExtrema [] = []
localExtrema [0,1,0] = [1]
localExtrema [1,0,1] = [0]
localExtrema [1,5,2,6,3,7] = [5,2,6,3]
localExtrema [1,2,3,4,5] = []
localExtrema [1,2,3,3,3,2,1] = []
```

Given a list of values of some type a that implements the Ord type class, the local extrema are the values that are either strictly bigger or strictly smaller than the numbers immediately before or after them. The goal of this question is to implement two different versions of the function localExtrema :: Ord a => [a] -> [a] that returns the list of all local extrema in a given list. The first and last elements of a list are never considered to be local extrema.
First, implement localExtrema using explicit pattern matching on lists and recursion. (Update: using guards or if then else is also allowed)
Clarification. Here you should only implement one version of localExtrema. The second one will be in subquestion 3C.

```haskell
localExtrema :: Ord a => [a] -> [a]
localExtrema [] = []
localExtrema [x] = []
localExtrema (x:[y]) = []
localExtrema (x:y:z:xs)
    | (y > x && y > z) || (y < x && y < z) = y:localExtrema (y:z: xs) 
    | otherwise = localExtrema (y:z:xs)


-- Test
prop_localExtrema_test1 :: Bool
prop_localExtrema_test1 = localExtrema [] == ([] :: [Int])

prop_localExtrema_test2 :: Bool
prop_localExtrema_test2 = localExtrema [0,1,0] == [1]

prop_localExtrema_test3 :: Bool
prop_localExtrema_test3 = localExtrema [1,0,1] == [0]

prop_localExtrema_test4 :: Bool
prop_localExtrema_test4 = localExtrema [1,5,2,6,3,7] == [5,2,6,3]

prop_localExtrema_test5 :: Bool
prop_localExtrema_test5 = localExtrema [1,2,3,4,5] == []

prop_localExtrema_test6 :: Bool
prop_localExtrema_test6 = localExtrema [1,2,3,3,3,2,1] == []
```

Next, implement a helper function triplets :: [a] -> [(a,a,a)] that computes the list of all adjacents triplets in the list. For example, triplets [1,2,3,4,5] = [(1,2,3),(2,3,4),(3,4,5)]. This function should be implemented using library functions on lists. It should NOT use pattern matching or list comprehensions.
Clarification. Your solution should also not use guards.

```haskell
triplets :: [a] -> [(a,a,a)]
triplets xs = map (\(c1, (c2, c3)) -> (c1, c2, c3)) (zip xs (zip (tail xs) (tail (tail xs))))

-- or

triplets :: [a] -> [(a,a,a)]
triplets xs = zip3 xs (drop 1 xs) (drop 2 xs) 

-- Test
prop_triplets_example :: Bool
prop_triplets_example = triplets [1,2,3,4,5] == [(1,2,3),(2,3,4),(3,4,5)]

triplets_spec :: [a] -> [(a,a,a)]
triplets_spec xs = zip3 xs (drop 1 xs) (drop 2 xs) 

prop_triplets_correct :: [Int] -> Property
prop_triplets_correct xs = triplets xs === triplets_spec xs

```

Now implement the second version of the function localExtrema', using a list comprehension together with the helper function triplets you defined in the previous part (you do not need to redefine it here). Your solution here should not use pattern matching or recursion.
Clarification. It is allowed to match on the result of a generator (on the left of an <-) in the list comprehension.

```haskell
localExtrema' :: Ord a => [a] -> [a]
localExtrema' xs = [ y | (x,y,z) <- triplets xs, (x < y && y > z) || (x > y && y < z) ]

-- Test
prop_localExtrema'_test1 :: Bool
prop_localExtrema'_test1 = localExtrema' [] == ([] :: [Int])

prop_localExtrema'_test2 :: Bool
prop_localExtrema'_test2 = localExtrema' [0,1,0] == [1]

prop_localExtrema'_test3 :: Bool
prop_localExtrema'_test3 = localExtrema' [1,0,1] == [0]

prop_localExtrema'_test4 :: Bool
prop_localExtrema'_test4 = localExtrema' [1,5,2,6,3,7] == [5,2,6,3]

prop_localExtrema'_test5 :: Bool
prop_localExtrema'_test5 = localExtrema' [1,2,3,4,5] == []

prop_localExtrema'_test6 :: Bool
prop_localExtrema'_test6 = localExtrema' [1,2,3,3,3,2,1] == []

localExtrema'_spec :: Ord a => [a] -> [a]
localExtrema'_spec xs = [ y | (x,y,z) <- triplets xs, (x < y && y > z) || (x > y && y < z) ]

prop_localExtrema'_correct :: [Int] -> Property
prop_localExtrema'_correct xs = localExtrema' xs === localExtrema'_spec xs
```

Finally, implement a QuickCheck test prop_localExtrema that tests that the two versions localExtrema and localExtrema' of the function always produce the same output (again, you do not need to redefine the functions here).

```haskell
prop_localExtrema :: [Int] -> Property
prop_localExtrema xs = localExtrema xs === localExtrema' xs
```
________________________________________________________________________________________________________________________________________

### Question 4 Data types and type classes

A trie is a data structure for efficiently representing a (finite) set of strings. In Haskell, we can define the datatype Trie as follows:
```haskell 
data Trie = Node Bool [(Char,Trie)] 
```
A trie Trie b ts describes the set of strings A such that:

The boolean b is true if the empty string "" belongs to A.
The list ts consists of the tuples ('c',t) such that (1) at least one string in the set starts with c, and (2) the subtrie t describes the set of strings s such that 'c':s is in the set A.
In particular, the list ts should contain at most one tuple (c,t) for each character c.

For example, the following is a trie that represents the set {"foo","bar","baz"}
```haskell
Node False [('f', Node False [('o', Node False [('o', Node True [])])])
           ,('b', Node False [('a', Node False [('r', Node True [])
                                               ,('z', Node True [])])])
           ]
```
For this assignment, you are asked to implement several functions on tries. Each function is described in its own sub-assignment.

First, define the trie myTrie :: Trie that represents the set {"","a","ab","ba"}.
```haskell
myTrie :: Trie
myTrie = Node True [ ('a', Node True  [('b', Node True [])])
                   , ('b', Node False [('a', Node True [])])
                   ]

-- Test
elemTrie_spec :: String -> Trie -> Bool
elemTrie_spec "" (Node b _) = b
elemTrie_spec (x:xs) (Node _ []) = False
elemTrie_spec (x:xs) (Node _ ts) = case lookup x ts of
  Just t  -> elemTrie_spec xs t
  Nothing -> False

prop_myTrie1 :: Bool
prop_myTrie1 = elemTrie_spec "" myTrie

prop_myTrie2 :: Bool
prop_myTrie2 = elemTrie_spec "a" myTrie

prop_myTrie3 :: Bool
prop_myTrie3 = elemTrie_spec "ab" myTrie

prop_myTrie4 :: Bool
prop_myTrie4 = elemTrie_spec "ba" myTrie

prop_myTrie_other :: String -> Bool
prop_myTrie_other s = elemTrie_spec s myTrie == (s == "" || s == "a" || s == "ab" || s == "ba")
```

Now implement a function singletonTrie :: String -> Trie that constructs a trie representing the singleton set {s} containing only the given string.

```haskell
singletonTrie :: String -> Trie
singletonTrie [] = Node True []
singletonTrie (x:xs) = Node False [(x, singletonTrie xs)]

-- Test
elemTrie_spec :: String -> Trie -> Bool
elemTrie_spec "" (Node b _) = b
elemTrie_spec (x:xs) (Node _ []) = False
elemTrie_spec (x:xs) (Node _ ts) = case lookup x ts of
  Just t  -> elemTrie_spec xs t
  Nothing -> False

prop_empty_elem :: Bool
prop_empty_elem = elemTrie_spec "" (singletonTrie "") == True

prop_empty_other :: String -> Bool
prop_empty_other s = elemTrie_spec s (singletonTrie "") == (s == "")

prop_char_elem :: Char -> Bool
prop_char_elem c = elemTrie_spec [c] (singletonTrie [c]) == True

prop_char_other :: Char -> Char -> Bool
prop_char_other c1 c2 = elemTrie_spec [c1] (singletonTrie [c2]) == (c1 == c2)

prop_singleton_elem :: Char -> Char -> String -> Bool
prop_singleton_elem c1 c2 s = elemTrie_spec (c1:c2:s) (singletonTrie (c1:c2:s)) == True

prop_singleton_other :: Char -> Char -> String -> String -> Bool
prop_singleton_other c1 c2 s1 s2 = elemTrie_spec s1 (singletonTrie (c1:c2:s2)) == (s1 == (c1:c2:s2))

```

Implement a function elemTrie :: String -> Trie -> Bool for checking whether a string belongs to the set defined by a given trie.

Hint. The standard prelude function lookup may come in handy.

```haskell
elemTrie :: String -> Trie -> Bool
elemTrie "" (Node b _) = b
elemTrie (x:xs) (Node _ []) = False
elemTrie (x:xs) (Node _ ts) = case lookup x ts of
  Just t  -> elemTrie xs t
  Nothing -> False
  
-- Test

instance Arbitrary Trie where
  arbitrary = sized trie'
    where
      trie' 0       = Node <$> arbitrary <*> pure []
      trie' n | n>0 = do
        b <- arbitrary
        len <- choose (0,n)
        let m = n `div` len
        let chars = take len ['a'..'z']
        subtries <- vectorOf len (trie' m)
        return $ Node b $ zip chars subtries
  
  shrink (Node b ts) = [Node False ts | b] ++ map (Node b) (shrinkList (\(c,t) -> map (\x -> (c,x)) (shrink t)) ts)
  
elemTrie_spec :: String -> Trie -> Bool
elemTrie_spec "" (Node b _) = b
elemTrie_spec (x:xs) (Node _ []) = False
elemTrie_spec (x:xs) (Node _ ts) = case lookup x ts of
  Just t  -> elemTrie_spec xs t
  Nothing -> False

singletonTrie_spec :: String -> Trie
singletonTrie_spec [] = Node True []
singletonTrie_spec (x:xs) = Node False [(x, singletonTrie_spec xs)]

nonEmptyTrie :: Trie -> Bool
nonEmptyTrie (Node b ts) = b || any nonEmptyTrie (map snd ts)
  
allStrings :: Trie -> Gen String
allStrings (Node b ts) = oneof $ [pure "" | b] ++ [ fmap (c:) (allStrings t) | (c,t) <- ts, nonEmptyTrie t ]

exampleTrie1 = 
  Node False [('f', Node False [('o', Node False [('o', Node True [])])])
             ,('b', Node False [('a', Node False [('r', Node True [])
                                                 ,('z', Node True [])])])
             ]

exampleTrie2 =
  Node True [ ('a', Node True  [('b', Node True [])])
            , ('b', Node False [('a', Node True [])])
            ]
            
prop_exampleTrie_elems = 
  "foo" `elemTrie` exampleTrie1 .&.
  "bar" `elemTrie` exampleTrie1 .&.
  "baz" `elemTrie` exampleTrie1 .&.
  ""    `elemTrie` exampleTrie2 .&.
  "a"   `elemTrie` exampleTrie2 .&.
  "ab"  `elemTrie` exampleTrie2 .&.
  "ba"  `elemTrie` exampleTrie2
  
prop_exampleTrie1_other :: String -> Property
prop_exampleTrie1_other s = elemTrie s exampleTrie1 === elemTrie_spec s exampleTrie1

prop_exampleTrie2_other :: String -> Property
prop_exampleTrie2_other s = elemTrie s exampleTrie2 === elemTrie_spec s exampleTrie2

prop_singletonTrie_elem :: String -> Bool
prop_singletonTrie_elem s = s `elemTrie` singletonTrie_spec s

prop_singletonTrie_other :: String -> String -> Property
prop_singletonTrie_other s1 s2 = (s1 `elemTrie` singletonTrie_spec s2) === (s1 == s2)

prop_elemTrie_elems :: Trie -> Property
prop_elemTrie_elems t = nonEmptyTrie t ==> forAll (allStrings t) (\x -> elemTrie x t)

prop_elemTrie_correct :: String -> Trie -> Property
prop_elemTrie_correct s t = elemTrie s t === elemTrie_spec s t

```

Define the empty trie emptyTrie :: Trie that represents the empty set {}, and the function mergeTrie :: Trie -> Trie -> Trie for merging two tries (taking the union of the two sets). Pay attention to the fact that the list of tuples in a trie should never contain two tuples with the same character as the first component!
Also make Trie into an instance of the Semigroup and Monoid type classes, using the functions you defined above.

```haskell
emptyTrie :: Trie
emptyTrie = Node False []

mergeTrie :: Trie -> Trie -> Trie
mergeTrie (Node b1 ts1) (Node b2 ts2) = 
  Node (b1 || b2) ([ (x,maybe t1 (mergeTrie t1) (lookup x ts2)) | (x,t1) <- ts1  ]
                ++ [ (x,t2) | (x,t2) <- ts2, isNothing (lookup x ts1) ])
  where
    isNothing Nothing = True
    isNothing _       = False

instance Semigroup Trie where
  (<>) = mergeTrie
  
instance Monoid Trie where
  mempty = emptyTrie
  mappend = (<>)
  
-- Test
instance Arbitrary Trie where
  arbitrary = sized trie'
    where
      trie' 0       = Node <$> arbitrary <*> pure []
      trie' n | n>0 = do
        b <- arbitrary
        len <- choose (0,n)
        let m = n `div` len
        let chars = take len ['a'..'z']
        subtries <- vectorOf len (trie' m)
        return $ Node b $ zip chars subtries
        
  shrink (Node b ts) = [Node False ts | b] ++ map (Node b) (shrinkList (\(c,t) -> map (\x -> (c,x)) (shrink t)) ts)

  
elemTrie_spec :: String -> Trie -> Bool
elemTrie_spec "" (Node b _) = b
elemTrie_spec (x:xs) (Node _ []) = False
elemTrie_spec (x:xs) (Node _ ts) = case lookup x ts of
  Just t  -> elemTrie_spec xs t
  Nothing -> False

singletonTrie_spec :: String -> Trie
singletonTrie_spec [] = Node True []
singletonTrie_spec (x:xs) = Node False [(x, singletonTrie_spec xs)]

mergeTrie_spec :: Trie -> Trie -> Trie
mergeTrie_spec (Node b1 ts1) (Node b2 ts2) = 
  Node (b1 || b2) ([ (x,maybe t1 (mergeTrie_spec t1) (lookup x ts2)) | (x,t1) <- ts1  ]
                ++ [ (x,t2) | (x,t2) <- ts2, isNothing (lookup x ts1) ])
  where
    isNothing Nothing = True
    isNothing _       = False
    
equalTrie_prop :: Trie -> Trie -> Bool
equalTrie_prop (Node b1 ts1) (Node b2 ts2) = b1 == b2 && sortByFst ts1 == sortByFst ts2
  where
  sortByFst = sortBy (\(c1,_) (c2,_) -> compare c1 c2)
  
prop_empty_elem :: String -> Bool
prop_empty_elem s = elemTrie_spec s emptyTrie == False

nonEmptyTrie :: Trie -> Bool
nonEmptyTrie (Node b ts) = b || any nonEmptyTrie (map snd ts)
  
allStrings :: Trie -> Gen String
allStrings (Node b ts) = oneof $ [pure "" | b] ++ [ fmap (c:) (allStrings t) | (c,t) <- ts, nonEmptyTrie t ]

prop_merge_simple :: Bool -> Bool -> Bool
prop_merge_simple b1 b2 = equalTrie_prop (mergeTrie (Node b1 []) (Node b2 [])) (Node (b1 || b2) [])

prop_merge_empty :: Trie -> Property
prop_merge_empty t = equalTrie_prop (mergeTrie emptyTrie t) t .&. equalTrie_prop (mergeTrie t emptyTrie) t

prop_merge_left :: Trie -> Trie -> Property
prop_merge_left t1 t2 = nonEmptyTrie t1 ==> forAll (allStrings t1) (\s -> elemTrie_spec s (mergeTrie t1 t2))

prop_merge_right :: Trie -> Trie -> Property
prop_merge_right t1 t2 = nonEmptyTrie t2 ==> forAll (allStrings t2) (\s -> elemTrie_spec s (mergeTrie t1 t2))

prop_merge_either :: Trie -> Trie -> Property
prop_merge_either t1 t2 = nonEmptyTrie t ==> forAll (allStrings t) (\s -> elemTrie_spec s t1 || elemTrie_spec s t2)
  where t = mergeTrie t1 t2

-- Leads to unpredictable results
--prop_merge_correct :: Trie -> Trie  -> Property
--prop_merge_correct t1 t2 = forAll arbitrary $ \s -> elemTrie_spec s (mergeTrie t1 t2) === (elemTrie_spec s t1 || elemTrie_spec s t2)

prop_merge_correct :: Trie -> Trie -> Bool
prop_merge_correct t1 t2 = equalTrie_prop (mergeTrie t1 t2) (mergeTrie_spec t1 t2)

prop_merge_correct_bis :: Trie -> Trie -> Bool
prop_merge_correct_bis t1 t2 = equalTrie_prop (mergeTrie t1 t2) (mergeTrie_spec t1 t2)

prop_merge_correct_tris :: Trie -> Trie -> Bool
prop_merge_correct_tris t1 t2 = equalTrie_prop (mergeTrie t1 t2) (mergeTrie_spec t1 t2)



prop_semigroup_correct :: Trie -> Trie -> Bool
prop_semigroup_correct t1 t2 = equalTrie_prop (t1 <> t2) (mergeTrie t1 t2)

prop_monoid_correct :: Trie -> Trie -> Property
prop_monoid_correct t1 t2 = 
  forAll (chooseInt (1,10)) $ \i ->
    if i == 1 then equalTrie_prop mempty emptyTrie
              else equalTrie_prop (t1 `mappend` t2) (mergeTrie t1 t2)

-- Additional tests (replacing prop_merge_correct)

prop_merge_singletons :: String -> String -> Property
prop_merge_singletons s1 s2 = s1 /= s2 ==> (not (equalTrie_prop t t1) && not (equalTrie_prop t t2))
  where
    t1 = singletonTrie_spec s1
    t2 = singletonTrie_spec s2
    t = mergeTrie t1 t2

prop_insert_singleton :: String -> Trie -> Property
prop_insert_singleton s t = elemTrie_spec s (mergeTrie (singletonTrie_spec s) t) .&. elemTrie_spec s (mergeTrie t (singletonTrie_spec s))

prop_merge_char :: Char -> Char -> Bool
prop_merge_char c1 c2 = equalTrie_prop (mergeTrie (tr [c1]) (tr [c2])) (tr (nub [c1,c2]))
  where
    tr cs = Node False (map (\c -> (c,Node True [])) cs)

prop_merge_chars :: Property
prop_merge_chars = 
  forAll (sublistOf ['a'..'m']) $ \cs1 ->
  forAll (sublistOf ['n'..'z']) $ \cs2 ->
  equalTrie_prop (mergeTrie (tr cs1) (tr cs2)) (tr (cs1++cs2))
  where
    tr cs = Node False (map (\c -> (c,Node True [])) cs)
```

State the three laws of the Monoid type class, and explain briefly why the first one is satisfied by your definition of Monoid Trie.

```haskell
The three laws are the following:

mempty <> t2 == t2
t1 <> mempty == t1
(t1 <> t2) <> t3 == t1 <> (t2 <> t3)
We can see that our implementation of Monoid Trie satisfies the first law as follows:

mempty <> Node b ts
= mergeTrie emptyTrie (Node b ts)
= mergeTrie (Node False []) (Node b ts)
= Node (False || b) ([ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- []  ]
                  ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- []  ]
       ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([] ++ [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ])
= Node b ([ (x,t2) | (x,t2) <- ts ])
= Node b ts
In words, we can note the following three facts:
1. The boolean component of mergeTrie emptyTrie (Node b ts) is False || b, which is equal to False.
2. The first list in the right-hand side becomes [ (x,maybe t1 (mergeTrie t1) (lookup x ts)) | (x,t1) <- [] ], which is empty.
3. The second list becomes [ (x,t2) | (x,t2) <- ts, isNothing (lookup x []) ], which is just ts since lookup x [] is always Nothing.
```
________________________________________________________________________________________________________________________________________

### Question 5 Functors, Applicative, Monads

Consider the following data type for keeping track of whether a value is “clean” or “dirty”:

```haskell 
data MaybeDirty a = Clean a | Dirty a 
```
First, define instances for making this type into a Functor, Applicative, and Monad. For the Applicative instance, pure should return a clean value, and the result of combining two values should be dirty if either of the values is dirty. For example:
```haskell
prop_ap_test1 = (pure (,) <*> Clean 1 <*> Clean 2)  ==  Clean (1,2)
prop_ap_test2 = (pure (,) <*> Dirty 1 <*> Clean 2)  ==  Dirty (1,2)
prop_ap_test3 = (pure (,) <*> Clean 1 <*> Dirty 2)  ==  Dirty (1,2)
prop_ap_test4 = (pure (,) <*> Dirty 1 <*> Dirty 2)  ==  Dirty (1,2)
```
Now use do-notation to implement a function addDirty :: (Ord a, Num a) => MaybeDirty a -> MaybeDirty a -> MaybeDirty a that adds together two values and marks the result as dirty if either one of the inputs is dirty, or if the result is negative. For example:
```haskell
addDirty (Dirty 1) (Clean 2) == Dirty 3
addDirty (Clean 1) (Clean 2) == Clean 3
addDirty (addDirty (Clean 1) (Clean (-2))) (Clean 3) == Dirty 2
```
Your implementation of addDirty should only use do notation and should not pattern match on the constructors Clean and Dirty directly or make use of helper functions that do so.

Clarification. Unfortunately there’s some syntax errors in the user tests (which cannot be updated at this point). Here is an updated version that you can copy-paste into the Test tab:
```haskell
prop_ap_test1 = (pure (,) <*> Clean 1 <*> Clean 2)  ==  Clean (1,2)
prop_ap_test2 = (pure (,) <*> Dirty 1 <*> Clean 2)  ==  Dirty (1,2)
prop_ap_test3 = (pure (,) <*> Clean 1 <*> Dirty 2)  ==  Dirty (1,2)
prop_ap_test4 = (pure (,) <*> Dirty 1 <*> Dirty 2)  ==  Dirty (1,2)

prop_addDirty_test1 = addDirty (Dirty 1) (Clean 2) == Dirty 3
prop_addDirty_test2 = addDirty (Clean 1) (Clean 2) == Clean 3
prop_addDirty_test3 = addDirty (addDirty (Clean 1) (Clean (-2))) (Clean 3) == Dirty 2
```


```haskell
markDirty :: MaybeDirty a -> MaybeDirty a
markDirty (Clean x) = Dirty x
markDirty (Dirty x) = Dirty x

instance Functor MaybeDirty where
  fmap f (Clean x) = Clean (f x)
  fmap f (Dirty x) = Dirty (f x)

instance Applicative MaybeDirty where
  pure x = Clean x
  Clean f <*> x = fmap f x 
  Dirty f <*> x = markDirty (fmap f x)

instance Monad MaybeDirty where
  return = pure
  Clean x >>= f = f x
  Dirty x >>= f = markDirty (f x)

addDirty :: (Ord a, Num a) => MaybeDirty a -> MaybeDirty a -> MaybeDirty a
addDirty mx my = do
  x <- mx
  y <- my
  if x + y < 0 then Dirty (x + y) else Clean (x + y)
```

-- Test:
```haskell
markDirty_spec :: MaybeDirty a -> MaybeDirty a
markDirty_spec (Clean x) = Dirty x
markDirty_spec (Dirty x) = Dirty x

fmap_spec f (Clean x) = Clean (f x)
fmap_spec f (Dirty x) = Dirty (f x)

pure_spec x = Clean x

ap_spec (Clean f) x = fmap_spec f x 
ap_spec (Dirty f) x = markDirty_spec (fmap_spec f x)

return_spec :: a -> MaybeDirty a
return_spec = pure_spec

bind_spec (Clean x) f = f x
bind_spec (Dirty x) f = markDirty_spec (f x)

addDirty_spec :: (Ord a, Num a) => MaybeDirty a -> MaybeDirty a -> MaybeDirty a
addDirty_spec mx my = do
  x <- mx
  y <- my
  if x + y < 0 then Dirty (x + y) else Clean (x + y)
  
instance Arbitrary a => Arbitrary (MaybeDirty a) where
  arbitrary = elements [Clean, Dirty] <*> arbitrary
  shrink (Clean x) = Clean <$> shrink x
  shrink (Dirty x) = Dirty <$> shrink x
  
-- Does not give consistent results
--prop_fmap_correct :: Fun Int Int -> MaybeDirty Int -> Property
--prop_fmap_correct (Fun _ f) x = fmap f x === fmap_spec f x

prop_fmap_clean :: Int -> Int -> Property
prop_fmap_clean x y = fmap (x+) (Clean y) === Clean (x+y)

prop_fmap_dirty :: Int -> Int -> Property
prop_fmap_dirty x y = fmap (x+) (Dirty y) === Dirty (x+y)

prop_pure_correct :: Int -> Property
prop_pure_correct x = pure x === pure_spec x

-- Does not give consistent results
--prop_ap_correct :: MaybeDirty (Fun Int Int) -> MaybeDirty Int -> Property
--prop_ap_correct f x = ((applyFun <$> f) <*> x) === ap_spec (applyFun <$> f) x

prop_ap_clean_clean :: Int -> Int -> Property
prop_ap_clean_clean x y = (Clean (x+) <*> Clean y) === Clean (x+y)

prop_ap_clean_dirty :: Int -> Int -> Property
prop_ap_clean_dirty x y = (Clean (x+) <*> Dirty y) === Dirty (x+y)

prop_ap_dirty_clean :: Int -> Int -> Property
prop_ap_dirty_clean x y = (Dirty (x+) <*> Clean y) === Dirty (x+y)

prop_ap_dirty_dirty :: Int -> Int -> Property
prop_ap_dirty_dirty x y = (Dirty (x+) <*> Dirty y) === Dirty (x+y)

prop_return_correct :: Int -> Property
prop_return_correct x = return x === return_spec x

-- Does not give consistent results
--prop_bind_correct :: MaybeDirty Int -> Fun Int (MaybeDirty Int) -> Property
--prop_bind_correct x f = (x >>= applyFun f) === bind_spec x (applyFun f)

prop_addDirty_correct :: MaybeDirty Int -> MaybeDirty Int -> Property
prop_addDirty_correct x y = addDirty x y === addDirty_spec x y

prop_bind_clean_clean :: Int -> Int -> Property
prop_bind_clean_clean x y = (Clean x >>= \z -> Clean (z+y)) === Clean (x+y)

prop_bind_clean_dirty :: Int -> Int -> Property
prop_bind_clean_dirty x y = (Clean x >>= \z -> Dirty (z+y)) === Dirty (x+y)

prop_bind_dirty_clean :: Int -> Int -> Property
prop_bind_dirty_clean x y = (Dirty x >>= \z -> Clean (z+y)) === Dirty (x+y)

prop_bind_dirty_dirty :: Int -> Int -> Property
prop_bind_dirty_dirty x y = (Dirty x >>= \z -> Dirty (z+y)) === Dirty (x+y) 
```

________________________________________________________________________________________________________________________________________

### Question 6 Laziness
Consider two definitions of the function fac :: Int -> Int:
```haskell
  fac 0 = 1
  fac n = n * fac (n - 1)
  
  fac' n = accum 1 n
    where
	  accum x 0 = x
	  accum x y = accum (x*y) (y-1)
```
Write down the evaluation sequences for fac 3 under innermost reduction and outermost reduction. If there are multiple valid redexes to choose, pick the leftmost one first. Can you see a difference in the performance between the two strategies in the number of evaluation steps or the size of the intermediate expressions? (4 points)

Write down the evaluation sequences for fac' 3 under innermost reduction and outermost reduction. If there are multiple valid redexes to choose, pick the leftmost one first. Can you see a difference in the performance between the two strategies in the number of evaluation steps or the size of the intermediate expressions? (4 points)

How would you modify the definition of fac' to improve its performance under the lazy evaluation strategy of Haskell? (2 points)

Innermost reduction of fac 3:
```haskell
fac 3 --> 3 * fac (3 - 1)
      --> 3 * fac 2
      --> 3 * (2 * fac (2 - 1))
      --> 3 * (2 * fac 1)
      --> 3 * (2 * (1 * fac (1 - 1)))
      --> 3 * (2 * (1 * fac 0)))
      --> 3 * (2 * (1 * 1)))
      --> 3 * (2 * 1)
      --> 3 * 2
      --> 6
```
Outermost reduction of fac 3:
```haskell
fac 3 --> 3 * fac (3 - 1)
      --> 3 * fac 2
      --> 3 * (2 * fac (2 - 1))
      --> 3 * (2 * fac 1)
      --> 3 * (2 * (1 * fac (1 - 1)))
      --> 3 * (2 * (1 * fac 0)))
      --> 3 * (2 * (1 * 1)))
      --> 3 * (2 * 1)
      --> 3 * 2
      --> 6
```
For fac, the choice of evaluation strategy does not matter.

Innermost reduction of fac' 3:
```haskell
fac' 3 --> accum 1 3
       --> accum (1*3) (3-1)
       --> accum 3 (3-1)
       --> accum 3 2
       --> accum (3*2) (2-1)
       --> accum 6 (2-1)
       --> accum 6 1
       --> accum (6*1) (1-1)
       --> accum 6 (1-1)
       --> accum 6 0
       --> 6
```
Outermost reduction of fac' 3:
```haskell
fac' 3 --> accum 1 3
       --> accum (1*3) (3-1)
       --> accum (1*3) 2
       --> accum ((1*3)*2) (2-1)
       --> accum ((1*3)*2) 1
       --> accum (((1*3)*2)*1) (1-1)
       --> accum (((1*3)*2)*1) 0
       --> ((1*3)*2)*1
       --> (3*2)*1
       --> 6*1
       --> 6
```
For fac' the number of evaluation steps is still the same under both strategies, but the size of intermediate expressions is much smaller under innermost reduction.

You can use the strict application operator ($!) to make accum strict in its first argument:
```haskell
  fac' n = accum 1 n
    where
	  accum x 0 = x
	  accum x y = (accum $! (x*y)) (y-1)
 ```
________________________________________________________________________________________________________________________________________

### Question 7 The Curry-Howard correspondence
Translate the following propositions to Agda types using the Curry-Howard correspondence:

If (not A) and (not B) then not (A or B)
If not (A or B) then (not A) and (not B)
Prove both statements by implementing an Agda function of the translated types.

Note. The unicode support in Weblab is not very good. We recommend you to either use an external editor, or use the variant names defined at the bottom of the library file (and use -> instead of →).

```haskell
open import library

proof1 : {A B : Set} → (A → ⊥) × (B → ⊥) → Either A B → ⊥
proof1 (f , g) (left x) = f x
proof1 (f , g) (right y) = g y

proof2 : {A B : Set} → (Either A B → ⊥) → (A → ⊥) × (B → ⊥)
proof2 f = (λ x → f (left x)) , (λ y → f (right y))
```

-- Test
```
open import Agda.Builtin.Equality

test-proof1-type : {A B : Set} → (A → ⊥) × (B → ⊥) → Either A B → ⊥
test-proof1-type = proof1

test-proof1-left : {A B : Set} (f : A → ⊥) (g : B → ⊥) (x : A) → proof1 (f , g) (left x) ≡ f x
test-proof1-left f g x = refl

test-proof1-right : {A B : Set} (f : A → ⊥) (g : B → ⊥) (y : B) → proof1 (f , g) (right y) ≡ g y
test-proof1-right f g x = refl

test-proof2-type : {A B : Set} → (Either A B → ⊥) → (A → ⊥) × (B → ⊥)
test-proof2-type = proof2

test-proof2-fst : {A B : Set} → (f : Either A B → ⊥) (x : A) → fst (proof2 f) x ≡ f (left x)
test-proof2-fst f x = refl

test-proof2-snd : {A B : Set} → (f : Either A B → ⊥) (y : B) → snd (proof2 f) y ≡ f (right y)
test-proof2-snd f y = refl
```
________________________________________________________________________________________________________________________________________

### Question 8 Equational reasoning

In the library code, there are definitions of the Agda types Nat and List as well as the functions _+_ on natural numbers, and _++_ and sum on lists. Your task is to use equational reasoning in Agda to prove that for any two lists xs and ys of type List Nat, we have sum (xs ++ ys) ≡ sum xs + sum ys.

Hint. For this proof, you will need to rely on the associativity of addition, which was proven in the lecture notes. The solution to that exercise has been copied in the library code for you, so you do not need to repeat the proof.

Note. The unicode support in Weblab is not very good. We recommend you to either use an external editor and copy-paste your solution here, or use the variant names defined at the bottom of the library file (and use -> instead of →).


```haskell
open import library

proof : (xs ys : List Nat) → sum (xs ++ ys) ≡ sum xs + sum ys
proof [] ys =
  begin
    sum ([] ++ ys)
  =⟨⟩
    sum ys
  =⟨⟩
    0 + sum ys
  =⟨⟩
    sum [] + sum ys
  end
proof (x :: xs) ys =
  begin
    sum ((x :: xs) ++ ys)
  =⟨⟩
    sum (x :: (xs ++ ys))
  =⟨⟩
    x + sum (xs ++ ys)
  =⟨ cong (x +_) (proof xs ys) ⟩
    x + (sum xs + sum ys)
  =⟨ add-assoc x (sum xs) (sum ys) ⟩
    (x + sum xs) + sum ys
  =⟨⟩
    sum (x :: xs) + sum ys
  end
```
-- Test
```haskell
open import library

test-proof-type : (xs ys : List Nat) → sum (xs ++ ys) ≡ sum xs + sum ys
test-proof-type = proof

test-proof-nil : proof [] [] ≡ refl
test-proof-nil = refl

test-proof-single : proof (1 :: []) (1 :: []) ≡ refl
test-proof-single = refl

test-proof-double : proof (1 :: 2 :: []) (1 :: 2 :: []) ≡ refl
test-proof-double = refl

test-proof-triple : proof (1 :: 2 :: 3 :: []) (1 :: 2 :: 3 :: []) ≡ refl
test-proof-triple = refl
```
-- Library
```haskell
data Nat : Set where
  zero : Nat
  suc  : (n : Nat) → Nat
{-# BUILTIN NATURAL Nat #-}

_+_ : Nat → Nat → Nat
zero  + m = m
suc n + m = suc (n + m)
infixl 6 _+_

data List (A : Set) : Set where
  []   : List A
  _::_ : A → List A → List A
infixr 5 _::_

_++_ : {A : Set} → List A → List A → List A
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

sum : List Nat → Nat
sum []        = 0
sum (x :: xs) = x + sum xs

data _≡_ {A : Set} : A → A → Set where
  refl : {x : A} → x ≡ x

infix 4 _≡_

-- symmetry of equality
sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- transitivity of equality
trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- congruence of equality
cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

begin_ : {A : Set} → {x y : A} → x ≡ y → x ≡ y
begin p = p

_end : {A : Set} → (x : A) → x ≡ x
x end = refl

_=⟨_⟩_ : {A : Set} → (x : A) → {y z : A}
       → x ≡ y → y ≡ z → x ≡ z
x =⟨ p ⟩ q = trans p q

_=⟨⟩_ : {A : Set} → (x : A) → {y : A} → x ≡ y → x ≡ y
x =⟨⟩ q = x =⟨ refl ⟩ q

infix   1  begin_
infix   3  _end
infixr  2  _=⟨_⟩_
infixr  2  _=⟨⟩_

add-assoc : (x y z : Nat) → x + (y + z) ≡ (x + y) + z
add-assoc zero y z =
  begin
    zero + (y + z)
  =⟨⟩                              -- applying the outer +
    y + z
  =⟨⟩                              -- unapplying add
    (zero + y) + z
  end
add-assoc (suc x) y z =
  begin
    (suc x) + (y + z)
  =⟨⟩                              -- applying the outer add
    suc (x + (y + z))
  =⟨ cong suc (add-assoc x y z) ⟩  -- using induction hypothesis
    suc ((x + y) + z)
  =⟨⟩                              -- unapplying the outer add
    (suc (x + y)) + z
  =⟨⟩                              -- unapplying the inner add
    ((suc x) + y) + z
  end
  
  
-- Use these type synonyms if you want to avoid unicode:

_==_ : {A : Set} → A → A →  Set
x == y = x ≡ y

_=<_>_ : {A : Set} → (x : A) → {y z : A} → x ≡ y → y ≡ z → x ≡ z
x =< p > q = x =⟨ p ⟩ q 

_=<>_ : {A : Set} → (x : A) → {y : A} → x ≡ y → x ≡ y
x =<> q = x =⟨⟩ q


infixr  2  _=<_>_
infixr  2  _=<>_
```

