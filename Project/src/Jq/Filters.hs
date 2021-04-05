module Jq.Filters where
import Data.List

data Filter = 
  -- Filters
    Identity
  | Parenthesis { parent :: Filter }                           -- Parenthesis
  | ObjIdx { objIdx :: String}                                 -- Object indexing
  | OptObjIdx { optObIdx :: String}                            -- Optional Object indexing
  | GenericObjIdx { genObjIdx :: [String]}                     -- Generic Object indexing
  | OptGenericObjIdx { genObjIdx :: [String]}                  -- Optional Generic Object Indexing
  | ArrIdx { arrIdx :: Int }                                   -- Array indexing
  | OptArrIdx { arrIdx :: Int }                                -- Optional Array indexing
  | Slicer {arrStart :: [Int], arrEnd :: [Int]}                    -- Array Slicer
  | OptSlicer {arrStart :: [Int], arrEnd :: [Int]}                 -- Optional Array Slicer
  | Iterator { iterIdx :: [Int]}                               -- Array/Object Value Iterator
  | OptIterator { iterIdx :: [Int]}                            -- Optional Array/Object Value Iterator
  | CommaOperator {comma1 :: Filter, comma2 :: Filter}               -- Comma Operator
  | PipeOperator {pipe1 :: Filter, pipe2 :: Filter}                  -- Pipe Operator
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
  show (Slicer start end) = ".[" ++ (intercalate (",") (map show start)) ++ ":" ++ (intercalate (",") (map show end)) ++ "]"
  show (OptSlicer start end) = ".[" ++ (intercalate (",") (map show start)) ++ ":" ++ (intercalate (",") (map show end)) ++ "]?"
  show (Iterator idxs) = ".[" ++ (intercalate (",") (map show idxs)) ++ "]"
  show (OptIterator idxs) = ".[" ++ (intercalate (",") (map show idxs)) ++ "]?"
  show (CommaOperator c1 c2) = show c1 ++ ", " ++ show c2
  show (PipeOperator p1 p2) = show p1 ++ " | " ++ show p2
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
