module Lec7 where

import Control.Monad
import Debug.Trace (trace)

import Data.List

{-
main = do
  colors <- sequence [getLine, getLine,getLine]
  sequence (map putStrLn (sort colors))
  return ()
-}

pad :: Int -> String -> String
pad n s 
  | length s < n = 
      let k = n - length s
      in  replicate k ' ' ++ s
  | otherwise = s

multtable :: [[Int]]
multtable =
  [ [ k*l | l <- [1..10] ] | k <- [1..10] ]

main = do
  let acts = map (map (putStr . pad 4 . show)) multtable :: [[IO ()]]
  sequence (map sequence acts) -- TODO: insert newlines
  return ()


fibonacci :: Int -> Int 
fibonacci 0 = trace "fibonacci 0" 1
fibonacci 1 = trace "fibonacci 1" 1
fibonacci n | n > 1 = 
  trace ("fibonacci " ++ show n) $
    fibonacci (n-1) + fibonacci (n-2)
