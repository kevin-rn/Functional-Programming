module Jq.Filters where
import Data.List
import Jq.Json (showJNumber, addSpace, showJSonChar)

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
  | Slicer {arrStart, arrEnd :: Filter}            -- Array Slicer
  | OptSlicer {arroptStart, arroptEnd :: Filter}   -- Optional Array Slicer
  | Iterator { iterIdx :: [Filter]}                          -- Array/Object Value Iterator
  | OptIterator { iterIdx :: [Filter]}                       -- Optional Array/Object Value Iterator
  | CommaOperator {commas :: [Filter]}                       -- Comma Operator
  | PipeOperator {pipes :: [Filter]}                         -- Pipe Operator
  -- Value constructors
  | ValueNull
  | ValueBool { boolean :: Bool }
  | ValueNumber { number :: Integer, fract :: [Int], exponential :: Integer }
  | ValueString { str :: String }
  | ValueArray { list :: [Filter] }
  | ValueObject { pairs :: [(Filter, Filter)]}
  -- Advanced Filters
  | RecursiveDescent
  | Equal { left, right :: Filter }
  | Unequal { left, right :: Filter }
  | IfElse { conditional, ifcase, elsecase :: Filter}
  | LowerThen { left, right :: Filter }
  | GreaterThen { left, right :: Filter }
  | LowerEq { left, right :: Filter }
  | GreaterEq { left, right :: Filter }
  | AndLogic { left, right :: Filter }
  | OrLogic { left, right :: Filter }
  | NotLogic

instance Show Filter where
  show (Identity)                = "."
  show (Parenthesis obj)         = "(" ++ show obj ++ ")"
  show (ObjIdx index)            = "." ++ show index
  show (OptObjIdx index)         = "." ++ show index ++ "?"
  show (GenericObjIdx index)     = ".[" ++ show index ++ "]"
  show (OptGenericObjIdx index)  = ".[" ++ show index ++ "]?"
  show (ArrIdx index)            = ".[" ++ show index ++ "]"
  show (OptArrIdx index)         = ".[" ++ show index ++ "]?"
  show (Slicer start end)        = ".[" ++ show start ++ ":" ++ show end ++ "]"
  show (OptSlicer start end)     = ".[" ++ show start ++ ":" ++ show end ++ "]?"
  show (Iterator idxs)           = ".[" ++ (intercalate (",") (map show idxs)) ++ "]"
  show (OptIterator idxs)        = ".[" ++ (intercalate (",") (map show idxs)) ++ "]?"
  show (CommaOperator commaList) = intercalate (",") (map show commaList)
  show (PipeOperator pipeList)   = intercalate ("|") (map show pipeList)
  -- Show value constructors
  show (ValueNull)               = "null"
  show (ValueString s)           = "\"" ++ concatMap showJSonChar s ++ "\""
  show (ValueNumber n xs e)      = showJNumber n xs e
  show (ValueBool True)          = "true"
  show (ValueBool False)         = "false"
  show (ValueArray a)            = case a of
      [] -> "[]"
      _ ->  addSpace ("[\n" ++ (intercalate (",\n") (map show a))) ++ "\n]"
  show (ValueObject o) = case o of 
      [] -> "{}" 
      _ -> addSpace ("{\n" ++ (intercalate (",\n") (map showPair o))) ++ "\n}"
        where
          showPair (k, v) = "\"" ++ show k ++ "\": " ++ show v
  show (RecursiveDescent)        = ".."
  show (Equal e1 e2)             = show e1 ++ " == " ++ show e2
  show (Unequal e1 e2)           = show e1 ++ " != " ++ show e2
  show (IfElse c e1 e2)          = "if " ++ show c ++ " then " ++ show e1 ++ " else " ++ show e2 ++ " end"
  show (LowerThen e1 e2)         = show e1 ++ " < " ++ show e2
  show (GreaterThen e1 e2)       = show e1 ++ " > " ++ show e2
  show (LowerEq e1 e2)           = show e1 ++ " <= " ++ show e2
  show (GreaterEq e1 e2)         = show e1 ++ " >= " ++ show e2
  show (AndLogic e1 e2)          = show e1 ++ " and " ++ show e2
  show (OrLogic e1 e2)           = show e1 ++ " or " ++ show e2
  show (NotLogic)                = "not"

data Config = ConfigC {filters :: Filter}
