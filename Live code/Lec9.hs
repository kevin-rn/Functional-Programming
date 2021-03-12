import Debug.Trace

lazySum :: [Int] -> Int -> Int 
lazySum []     acc = acc
lazySum (x:xs) acc = trace "recursing" (lazySum xs (add x acc))
  where
    add x y = trace ("adding " ++ show x ++ " to " ++ show y) (x+y)

strictSum :: [Int] -> Int -> Int 
strictSum []     acc = acc
strictSum (x:xs) acc = trace "recursing" (strictSum xs $! add x acc)
  where
    add x y = trace ("adding " ++ show x ++ " to " ++ show y) (x+y)
