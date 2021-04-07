module Jq.Json where
import Data.List

data JSON =
    JNull
  | JString { str :: String }
  | JNumber { number :: Integer, fract :: [Int], exp :: Integer }
  | JBool { boolean :: Bool } 
  | JObject { pairs :: [(String, JSON)]}
  | JArray { list :: [JSON] }

instance Show JSON where
  show (JNull) = "null"
  show (JString s) = "\"" ++ concatMap showJSonChar s ++ "\""
  show (JNumber n xs e) = showJNumber n xs e
  show (JBool True)  = "true"
  show (JBool False) = "false"
  show (JArray a) = case a of
      [] -> "[]"
      _ ->  addSpace ("[\n" ++ (intercalate (",\n") (map show a))) ++ "\n]"
  show (JObject o) = case o of 
      [] -> "{}" 
      _ -> addSpace ("{\n" ++ (intercalate (",\n") (map showPair o))) ++ "\n}"
        where
          showPair (k, v) = "\"" ++ concatMap showJSonChar k ++ "\": " ++ show v

instance Eq JSON where
  (JString a) == (JString b) = a == b
  (JNumber a f1 e1) == (JNumber b f2 e2) = a == b && f1 == f2 && e1 == e2
  (JBool a) == (JBool b) = a == b
  (JObject a) == (JObject b) = a == b
  (JArray a) == (JArray b) = a == b
  JNull == JNull = True 
  _ == _ = False

showJNumber :: Integer -> [Int] -> Integer -> String
showJNumber n [] 0 = show n
showJNumber n xs 0 = show n ++ "." ++ concatMap show xs
showJNumber n [] e = show n ++ "e" ++ show e
showJNumber n xs e = show n ++ "." ++concatMap show xs ++ "e" ++ show e


showJSonChar :: Char -> String
showJSonChar '\b' = "\\b"     -- backspace
showJSonChar '\f' = "\\f"     -- form feed
showJSonChar '\n' = "\\n"     -- newline
showJSonChar '\r' = "\\r"     -- carriage return
showJSonChar '\t' = "\\t"     -- tab
showJSonChar '\'' = "'"       -- reverse solidus
showJSonChar '\\' = "\\\\"
showJSonChar '\"' =  "\\\""   -- quotation mark
showJSonChar '/' = "\\/"      -- solidus
showJSonChar s = [s]

addSpace :: String -> String
addSpace ('\n':xs) = "\n  " ++ addSpace xs 
addSpace (x:xs) = [x] ++ addSpace xs
addSpace _ = []