module Jq.Filters where

data Filter = 
    Identity
  | Parenthesis { parent :: Filter }                           -- Parenthesis
  | ObjIdx { objIdx :: String}                                 -- Object indexing
  | OptObjIdx { optObIdx :: String}                            -- Optional Object indexing
  | GenericObjIdx { genObjIdx :: [String]}                     -- Generic Object indexing
  | OptGenericObjIdx { genObjIdx :: [String]}                  -- Optional Generic Object Indexing
  | ArrIdx { arrIdx :: Int }                                   -- Array indexing
  | OptArrIdx { arrIdx :: Int }                                -- Optional Array indexing
  | Slicer {arrStart :: Int, arrEnd :: Int}                    -- Array Slicer
  | OptSlicer {arrStart :: Int, arrEnd :: Int}                 -- Optional Array Slicer
  | Iterator { iterIdx :: [Int]}                               -- Array/Object Value Iterator
  | OptIterator { iterIdx :: [Int]}                            -- Optional Array/Object Value Iterator
  | CommaOperator {comma1 :: Filter, comma2 :: Filter}               -- Comma Operator
  | PipeOperator {pipe1 :: Filter, pipe2 :: Filter}                  -- Pipe Operator

instance Show Filter where
  show (Identity) = "."
  show (Parenthesis obj) = "(" ++ show obj ++ ")"
  -- show (ObjIdx o) = 

data Config = ConfigC {filters :: Filter}
