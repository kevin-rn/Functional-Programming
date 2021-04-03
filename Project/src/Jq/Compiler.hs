module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (Identity) inp = return [inp]
compile (Parenthesis obj) inp = compile obj inp
-- Handle (optional) Object indexing
compile (ObjIdx idx) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj]
    _ -> Left "Error: Cannot index non-JSON object using string"
compile (OptObjIdx idx) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj]
    _ -> Right []
-- Handle (optional) Generic Object indexing
compile (GenericObjIdx idxs) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj | idx <- idxs]
    _ -> Left "Error: Cannot index non-JSON object using string"
compile (OptGenericObjIdx idxs) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj | idx <- idxs]
    _ -> Right []
-- Handle (optional) Array indexing
compile (ArrIdx idx) inp = case inp of
    JArray arr ->  Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Left "Error: Cannot index non-array"
compile (OptArrIdx idx) inp = case inp of
    JArray arr -> Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Right []
-- Handle (optional) Array/String Slicing
compile (Slicer start end) inp = case inp of
    JArray arr -> let len = (length arr) in Right [getSlice (checkNegIdx start len) (checkNegIdx end len) inp]
    JString chars -> let len = (length chars) in Right [getSlice (checkNegIdx start len) (checkNegIdx end len) inp]
    _ -> Left "Error: Cannot slice non-array/non-object"
compile (OptSlicer start end) inp = case inp of
    JArray arr -> let len = (length arr) in Right [getSlice (checkNegIdx start len) (checkNegIdx end len) inp]
    JString chars -> let len = (length chars) in Right [getSlice (checkNegIdx start len) (checkNegIdx end len) inp]
    _ -> Right []
-- Handle (optional) Array/Object Iterator
compile (Iterator idxs) inp = case inp of
    JArray arr ->  if idxs == [] then Right (arr) 
                   else Right [getArrayValue idx arr | idx <- idxs]
    JObject obj -> if idxs == [] then Right (map snd obj) 
                   else Left "Error: cannot index object with number"
    _ -> Left "Error: Cannot iterate over non-array/non-object "
compile (OptIterator idxs) inp = case inp of
    JArray arr -> if idxs == [] then Right (arr) 
                  else Right [getArrayValue idx arr | idx <- idxs]
    JObject obj -> if idxs == [] then Right (map snd obj) 
                   else Right []
    _ -> Right []
-- Handle Comma and Pipe operator
compile (CommaOperator c1 c2) inp = case ((compile c1 inp), (compile c2 inp)) of
    (Right result1, Right result2) -> Right (result1 ++ result2)
    (_            , _            ) -> Left "Error: Cannot parse improper comma format"
compile (PipeOperator p1 p2) inp = case (compile p1 inp) of
    Right result1 -> handlePipes (compile p2) result1
    _ -> Left "Error: Cannot parse improper pipe format"

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

-- Check if negative is negative, if so then offset the index with a certain length to be positive else return the original index.
checkNegIdx :: Int -> Int -> Int
checkNegIdx idx len = if idx < 0 then len + idx else idx

-- Get the value in the object at a certain index or return null.
getObjectValue :: String -> [(String, JSON)] -> JSON
getObjectValue _ [] = JNull
getObjectValue idx ((key, value):xs) = if key == idx then value else getObjectValue idx xs

-- Get the value in the array at a certain index or return null.
getArrayValue :: Int -> [JSON] -> JSON
getArrayValue _ [] = JNull
getArrayValue idx (x:xs) = if idx == 0 then x else getArrayValue (idx - 1) xs

-- Get the values(s) in the range start to end within the string or array.
getSlice :: Int -> Int -> JSON -> JSON
getSlice    _     _   (JString "") = JString ""
getSlice    _     _   (JArray []) = JArray []
getSlice    start end (JString chars) = if end > start then JString (take (end - start) (drop start chars)) else JString ""
getSlice    start end (JArray arr) = if end > start then JArray (take (end - start) (drop start arr)) else JArray []

handlePipes :: (JProgram [JSON]) -> [JSON] -> Either String [JSON]
handlePipes _ [] = Left "Error: Cannot parse improper pipe format"
handlePipes f (x:xs) = case (f x) of 
    Right value -> case (handlePipes f xs) of 
        Right values -> Right (value ++ values)
        Left msg -> Left msg
    Left msg -> Left msg