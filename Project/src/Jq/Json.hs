module Jq.Json where
import Data.List
import Numeric (showHex)
import Data.Char (ord)

data JSON =
    JNull
  | JBool { boolean :: Bool }
  | JNumber { number :: Integer, fract :: [Int], exp :: Integer }
  | JString { str :: String }
  | JObject { pairs :: [(String, JSON)]}
  | JArray { list :: [JSON] }

-- Instance for Equality
instance Eq JSON where
  JNull              == JNull              = True
  (JBool b1)         == (JBool b2)         = b1 == b2
  (JNumber n1 f1 e1) == (JNumber n2 f2 e2) = (n1 == n2) && (f1 == f2) && (e1 == e2)
  (JString s1)       == (JString s2)       = s1 == s2
  (JArray a1)        == (JArray a2)        = a1 == a2
  (JObject o1)       == (JObject o2)       = o1 == o2
  _                  == _                  = False

-- Instance for Order
instance Ord JSON where
  JNull               <= JNull              = True
  JNull               <= JBool{}            = True
  JNull               <= JNumber{}          = True
  JNull               <= JString{}          = True
  JNull               <= JArray{}           = True
  JNull               <= JObject{}          = True
  (JBool b1)          <= (JBool b2)         = b1 <= b2
  JBool{}             <= JString{}          = True
  JBool{}             <= JNumber{}          = True
  JBool{}             <= JArray{}           = True
  JBool{}             <= JObject{}          = True
  (JNumber n1 f1 e1)  <= (JNumber n2 f2 e2) = (n1 <= n2) && (f1 <= f2) && (e1 <= e2)
  JNumber{}           <= JString{}          = True
  JNumber{}           <= JArray{}           = True
  JNumber{}           <= JObject{}          = True
  (JString s1)        <= (JString s2)       = s1 <= s2
  JString{}           <= JArray{}           = True
  JString{}           <= JObject{}          = True
  (JArray a1)         <= (JArray a2)        = a1 <= a2
  JArray{}            <= JObject{}          = True
  (JObject o1)        <= (JObject o2)       = o1 <= o2
  _                   <= _                  = False

--Instance for show
instance Show JSON where
  show (JNull)          = "null"
  show (JString s)      = "\"" ++ concatMap showJSonChar s ++ "\""
  show (JNumber n xs e) = showJNumber n xs e
  show (JBool True)     = "true"
  show (JBool False)    = "false"
  show (JArray a)       = case a of
      [] -> "[]"
      _ ->  addSpace ("[\n" ++ (intercalate (",\n") (map show a))) ++ "\n]"
  show (JObject o)      = case o of 
      [] -> "{}" 
      _ -> addSpace ("{\n" ++ (intercalate (",\n") (map showPair o))) ++ "\n}"
        where
          showPair (k, v) = "\"" ++ concatMap showJSonChar k ++ "\": " ++ show v

-- Helper method for properly formatting the JSON number.
showJNumber :: Integer -> [Int] -> Integer -> String
showJNumber n [] 0 = show n
showJNumber n xs 0 = show n ++ "." ++ concatMap show xs
showJNumber n [] e = show n ++ "e" ++ show e
showJNumber n xs e = show n ++ "." ++concatMap show xs ++ "e" ++ show e

-- Helper method for properly escaping some JSON characters.
-- TODO: Include handling unicode characters
showJSonChar :: Char -> String
showJSonChar '\b' = "\\b"     -- backspace
showJSonChar '\f' = "\\f"     -- form feed
showJSonChar '\n' = "\\n"     -- newline
showJSonChar '\r' = "\\r"     -- carriage return
showJSonChar '\t' = "\\t"     -- tab
showJSonChar '\'' = "'"       -- reverse solidus
showJSonChar '\\' = "\\\\"    -- slash
showJSonChar '\"' =  "\\\""   -- quotation mark
showJSonChar '/' = "\\/"      -- solidus
showJSonChar s
  | s `elem` ['\0' .. '\31'] = let chars = "0000" ++ (showHex (ord s) "") in "\\u" ++ (drop (length chars - 4) chars)  --Unicode character
  | otherwise                = [s]          -- string

-- Used to pretty print the strings of the Show method.
addSpace :: String -> String
addSpace ('\n':xs) = "\n  " ++ addSpace xs 
addSpace (x:xs) = [x] ++ addSpace xs
addSpace _ = []