module Jq.Filters where
import Data.List

data Filter = 
  -- Filters
    Identity
  | Parenthesis { parent :: Filter }                         -- Parenthesis
  | ObjIdx { objIdx :: String}                               -- Object indexing
  | OptObjIdx { optObIdx :: String}                          -- Optional Object indexing
  | GenericObjIdx { genObjIdx :: Filter}                     -- Generic Object indexing
  | OptGenericObjIdx { genObjIdx :: Filter}                  -- Optional Generic Object Indexing
  | ArrIdx { arrIdx :: Int }                                 -- Array indexing
  | OptArrIdx { arrIdx :: Int }                              -- Optional Array indexing
  | Slicer {arrStart :: Filter, arrEnd :: Filter}            -- Array Slicer
  | OptSlicer {arroptStart :: Filter, arroptEnd :: Filter}   -- Optional Array Slicer
  | Iterator { iterIdx :: [Filter]}                            -- Array/Object Value Iterator
  | OptIterator { iterIdx :: [Filter]}                         -- Optional Array/Object Value Iterator
  | CommaOperator {commas :: [Filter]}                       -- Comma Operator
  | PipeOperator {pipes :: [Filter]}                         -- Pipe Operator
  -- Value constructors
  | ValueNull
  | ValueBool { boolean :: Bool }
  | ValueNumber { number :: Integer, fract :: [Int], exponential :: Integer }
  | ValueString { str :: String }
  | ValueArray { list :: [Filter] }
  | ValueObject { pairs :: [(Filter, Filter)]}

instance Show Filter where
  show (Identity) = "."
  show (Parenthesis obj) = "(" ++ show obj ++ ")"
  show (ObjIdx index) = "." ++ show index
  show (OptObjIdx index) = "." ++ show index ++ "?"
  show (GenericObjIdx index) = ".[" ++ show index ++ "]"
  show (OptGenericObjIdx index) = ".[" ++ show index ++ "]?"
  show (ArrIdx index) = ".[" ++ show index ++ "]"
  show (OptArrIdx index) = ".[" ++ show index ++ "]?"
  show (Slicer start end) = ".[" ++ show start ++ ":" ++ show end ++ "]"
  show (OptSlicer start end) = ".[" ++ show start ++ ":" ++ show end ++ "]?"
  show (Iterator idxs) = ".[" ++ (intercalate (",") (map show idxs)) ++ "]"
  show (OptIterator idxs) = ".[" ++ (intercalate (",") (map show idxs)) ++ "]?"
  show (CommaOperator commaList) = intercalate (",") (map show commaList)
  show (PipeOperator pipeList) = intercalate ("|") (map show pipeList)
  -- Show value constructors
  show (ValueNull) = "null"
  show (ValueString s) = "\"" ++ concatMap showFilterChar s ++ "\""
  show (ValueNumber n xs e) = showValueNumber n xs e
  show (ValueBool True)  = "true"
  show (ValueBool False) = "false"
  show (ValueArray a) = case a of
      [] -> "[]"
      _ ->  addSpace ("[\n" ++ (intercalate (",\n") (map show a))) ++ "\n]"
  -- show (ValueObject o) = case o of 
  --     [] -> "{}" 
  --     _ -> addSpace ("{\n" ++ (intercalate (",\n") (map showPair o))) ++ "\n}"
  --       where
  --         showPair (k, v) = "\"" ++ concatMap showFilterChar k ++ "\": " ++ show v

showValueNumber :: Integer -> [Int] -> Integer -> String
showValueNumber n [] 0 = show n
showValueNumber n xs 0 = show n ++ "." ++ concatMap show xs
showValueNumber n [] e = show n ++ "e" ++ show e
showValueNumber n xs e = show n ++ "." ++ concatMap show xs ++ "e" ++ show e

showFilterChar :: Char -> String
showFilterChar '\b' = "\\b"     -- backspace
showFilterChar '\f' = "\\f"     -- form feed
showFilterChar '\n' = "\\n"     -- newline
showFilterChar '\r' = "\\r"     -- carriage return
showFilterChar '\t' = "\\t"     -- tab
showFilterChar '\'' = "'"       -- reverse solidus
showFilterChar '\\' = "\\\\"
showFilterChar '\"' =  "\\\""   -- quotation mark
showFilterChar '/' = "\\/"      -- solidus
showFilterChar s = [s]

addSpace :: String -> String
addSpace ('\n':xs) = "\n  " ++ addSpace xs 
addSpace (x:xs) = [x] ++ addSpace xs
addSpace _ = []

data Config = ConfigC {filters :: Filter}
