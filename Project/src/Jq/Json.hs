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
  show (JNumber n xs e) = showJNumber (JNumber n xs e)
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

showJNumber :: JSON -> String
showJNumber (JNumber n [] 0) = show n
showJNumber (JNumber n xs 0) = show n ++ "." ++ concatMap show xs
showJNumber (JNumber n [] e) = show n ++ "e" ++ show e
showJNumber (JNumber n xs e) = show n ++ "." ++concatMap show xs ++ "e" ++ show e
showJNumber _ = ""

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