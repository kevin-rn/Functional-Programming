module Jq.Compiler where

import           Jq.Filters
import           Jq.Json


type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
compile (ValueNull) _ = Right [JNull]
compile (ValueBool bool) _ = Right [JBool bool]
compile (ValueNumber num frac expo) _ = Right [JNumber num frac expo]
compile (ValueString chars) _ = Right [JString chars]
compile (ValueArray arr) inp = case (getArrayElements arr inp) of
    Right elements -> Right [JArray elements]
    Left msg -> Left msg
compile (ValueObject obj) _ = Right [JNull]
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
compile (GenericObjIdx idx) inp = case (compile idx inp) of
    Left msg -> Left msg
    Right indices -> case inp of 
        JObject obj -> (getGenObjectValue indices obj False)
        _ -> Left "Error: Cannot index non-JSON object"
compile (OptGenericObjIdx idx) inp = case (compile idx inp) of
    Left msg -> Left msg
    Right indices -> case inp of 
        JObject obj -> (getGenObjectValue indices obj True)
        _ -> Right []

-- Handle (optional) Array indexing
compile (ArrIdx idx) inp = case inp of
    JArray arr ->  Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Left "Error: Cannot index non-array"
compile (OptArrIdx idx) inp = case inp of
    JArray arr -> Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Right []

-- Handle (optional) Array/String Slicing
compile (Slicer start end) inp = case (compile start inp, compile end inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right first, Right second) -> case inp of
        JArray _ -> flattenJProgram [getSlice s e inp False | (s, e) <- getCombinations first second]
        JString _ -> flattenJProgram [getSlice s e inp False | (s, e) <- getCombinations first second]
        _ -> Left "Error: Cannot slice non-array/non-object"
compile (OptSlicer start end) inp = case (compile start inp, compile end inp) of
    (Right first, Right second) -> case inp of
        JArray _ -> flattenJProgram [getSlice s e inp True | (s, e) <- getCombinations first second]
        JString _ -> flattenJProgram [getSlice s e inp True | (s, e) <- getCombinations first second]
        _ -> Right []
    (_, _) -> Right []
    
-- Handle (optional) Array/Object Iterator
compile (Iterator idxs) inp = case idxs of
    [] -> case inp of
        JArray arr -> Right arr
        JObject obj -> Right (map snd obj) 
        _ -> Left "Error: Cannot iterate over non-array/non-object "
    _ -> case (compile (head idxs) inp) of 
        Left msg -> Left msg
        Right indices -> case inp of
            JArray arr -> getIterateValue indices arr False
            JObject obj -> Left "Error: cannot do partial iteration over object"
            _ -> Left "Error: Cannot iterate over non-array/non-object"
compile (OptIterator idxs) inp = case idxs of
    [] -> case inp of
        JArray arr -> Right arr
        JObject obj -> Right (map snd obj) 
        _ -> Right []
    _ -> case (compile (head idxs) inp) of 
        Left msg -> Left msg
        Right indices -> case inp of
            JArray arr -> getIterateValue indices arr False
            _ -> Right []
-- Handle Comma and Pipe operator
compile (CommaOperator commaList) inp = handleCommas commaList inp
compile (PipeOperator (pipe:pipeList)) inp = case compile pipe inp of
    Right result -> handlePipes pipeList result
    Left msg -> Left msg

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

-- Gets all elements of the array
getArrayElements :: [Filter]-> JSON -> Either String [JSON]
getArrayElements [] _ = Right []
getArrayElements (x:xs) inp = case (compile x inp) of
    Right result1 -> case (getArrayElements xs inp) of
        Right result2 -> Right (result1 ++ result2)
        Left msg -> Left msg
    Left msg -> Left msg

-- Converts string to floating point number
convertNumber :: String -> Float
convertNumber num = read num :: Float

-- Check if negative is negative, if so then offset the index with a input length.
-- Check second time if new index is positive and return else set it to 0.
checkNegIdx :: Int -> Int -> Int
checkNegIdx idx len = if idx < 0 then let newidx = len + idx in if newidx < 0 then 0 else newidx
                      else idx

-- Get the value in the object at a certain index or return null.
-- Input:   Key string   Object (list of key-value pair)
getObjectValue :: String -> [(String, JSON)] -> JSON
getObjectValue _ [] = JNull
getObjectValue idx ((key, value):xs) = if key == idx then value else getObjectValue idx xs

-- Get the values in the object at certain indices
-- Input:   list of JSON types - Object (list of key-value pair) - Boolean to indicate optional '?'
getGenObjectValue :: [JSON]-> [(String, JSON)] -> Bool -> Either String [JSON]
getGenObjectValue [] _ _= Right []
getGenObjectValue [(JString key)] obj _ = Right [getObjectValue key obj]
getGenObjectValue ((JString key):xs) obj bool= case (getGenObjectValue xs obj bool) of
    Right results -> Right ([getObjectValue key obj] ++ results)
    Left msg -> Left msg
getGenObjectValue _ _ False = Left "Error: Cannot index object with non-string indexing"
getGenObjectValue _ _ True = Right []

-- Get the value in the array at a certain index or return null.
getArrayValue :: Int -> [JSON] -> JSON
getArrayValue _ [] = JNull
getArrayValue idx (x:xs) = if idx == 0 then x else getArrayValue (idx - 1) xs

-- TODO: Fix this method to correctly find the indices
-- Input:  original sublist - list to check -
findIndexSubList :: [JSON] -> [JSON] -> Int
findIndexSubList _ [] = -1
findIndexSubList [] _ = -1
findIndexSubList as list@(x:xs)
                | all (uncurry (==)) $ zip as list = 0
                | otherwise = 1 + findIndexSubList as xs

-- Gets all values from iterator
-- Input:   list of JSON types - Input - Boolean to indicate optional '?'
getIterateValue :: [JSON] -> [JSON] -> Bool-> Either String [JSON]
getIterateValue [] input _ = Right input
getIterateValue [(idx@JNumber{})] input _ = let newidx = round (convertNumber (show idx)) in Right [getArrayValue newidx input]
getIterateValue ((idx@JNumber{}):xs) input bool = case (getIterateValue xs input bool) of
    Right values -> let newidx = round (convertNumber (show idx)) in Right ([getArrayValue newidx input] ++ values)
    Left msg -> Left msg
getIterateValue [(JArray arr)] input _ = let index = (findIndexSubList arr input) in if index == -1 then Right [(JArray [])] else Right [(JArray [JNumber (toInteger index) [] 0])]
getIterateValue ((JArray arr):xs) input bool = case (getIterateValue xs input bool) of
    Right values -> let index = (findIndexSubList arr input) in if index == -1 then Right ([(JArray [])] ++ values) else Right ([JArray [JNumber (toInteger  index) [] 0]] ++ values)
    Left msg -> Left msg
getIterateValue _ _ True = Right []
getIterateValue _ _ False = Left "Error: Cannot index array with non-integer indexing"

-- Generic combiner of elements of first and second list to create cartesian product list.
getCombinations :: [a] -> [a] -> [(a, a)]
getCombinations xs ys = [(x,y) | x <- xs, y <- ys]

-- Get the values(s) in the range start to end within the string or array.
-- Input: Starting index - End index - Input - Boolean to indicate optional '?'
getSlice :: JSON -> JSON -> JSON -> Bool -> Either String [JSON]
getSlice JNumber{} JNumber{} (JString "") _ = Right [JString ""]
getSlice JNumber{} JNumber{} (JArray []) _ = Right [JArray []]
getSlice start@JNumber{} end@JNumber{} (JString chars) _ = let
    len = (length chars)
    startidx = checkNegIdx (floor (convertNumber (show start))) len
    endidx = checkNegIdx (ceiling (convertNumber (show end))) len
    in if endidx > startidx then Right [JString (take (endidx - startidx) (drop startidx chars))]
       else Right [JString ""]
getSlice start@JNumber{} end@JNumber{} (JArray arr) _ = let
    len = (length arr)
    startidx = checkNegIdx (floor (convertNumber (show start))) len
    endidx = checkNegIdx (ceiling (convertNumber (show end))) len
    in if endidx > startidx then Right [JArray (take (endidx - startidx) (drop startidx arr))]
       else Right [JArray []]
-- cases for handling Null
getSlice JNull     end      input       bool  = let lowerbound = (JNumber 0 [] 0) in (getSlice lowerbound end input bool)
getSlice start     JNull    input       bool  = let upperbound = (JNumber ((fromIntegral (maxBound :: Int)) `div` 2) [] 0) in (getSlice start upperbound input bool)
-- cases for if the indices are neither a number or null or the correct input format
getSlice _         _        _           True  = Right []
getSlice _         _        (JString _) False = Left "Error: Start and end indices of an string slice must be numbers"
getSlice _         _        (JArray _) False = Left "Error: Start and end indices of an array slice must be numbers"
getSlice _         _         _          False = Left "Error: Cannot slice non-string/non-array"

-- Takes a list of Either objects and flattens it to a single Either object containing a Right with all values or the first Left encountered
flattenJProgram :: [Either String [JSON]] -> Either String [JSON]
flattenJProgram [] = Right []
flattenJProgram ((Left msg):_) = Left msg
flattenJProgram ((Right result):xs) = case (flattenJProgram xs) of
    Right results -> Right (result ++ results)
    Left msg -> Left msg

-- Helper method for compiling Comma Operator
-- Input: List of filters - input JSON
handleCommas :: [Filter] -> JSON -> Either String [JSON]
handleCommas [] _ = Left "Error: Cannot parse improper comma format"
handleCommas [comma] input = compile comma input
handleCommas (comma:commaList) input = case (compile comma input) of
    Right result -> case (handleCommas commaList input) of
        Right results -> Right (result ++ results)
        Left msg -> Left msg
    Left msg -> Left msg

-- Helper methods for compiling Pipe Operator
-- Input: List of filters - input JSON list
handlePipes :: [Filter] -> [JSON] -> Either String [JSON]
handlePipes [] _ = Left "Error: Cannot parse improper pipe format"
handlePipes [x] input = handleInput (compile x) input
handlePipes (x:xs) input = case (handleInput (compile x) input) of 
    Right result -> (handlePipes xs result)
    Left msg -> Left msg

-- Handles all the JSON in the input list and returns a single Either
-- Input: single filter - input JSON list
handleInput :: (JProgram [JSON]) -> [JSON] -> Either String [JSON]
handleInput _ [] = Left "Error: Cannot parse improper format"
handleInput f [x] = f x
handleInput f (x:xs) = case (f x) of 
    Right result -> case (handleInput f xs) of 
        Right results -> Right (result ++ results)
        Left msg -> Left msg
    Left msg -> Left msg

-- echo '[[4, 5], 1, 2, "bruh", 4, 5]' | jq '.[.[0], .[1]]?'
-- =
-- [4]
-- 1

-- echo '[1, 2, 3]' | jq '.[[1, 2]]'
-- [0]

-- echo '[0, 1, 2, "lee", "ghandi", 5, "lee", "ghandi"]' | jq '.[["lee", "ghandi"]]'
-- [3, 6]

-- echo '[[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]]' | jq-clone '.[][1]["name"], .[]'

-- echo '[[{"name":"JSON", "good":true}, {"name":"XML", "good":false}]]' | jq-clone '.[][1]["name"]'

-- echo '4' | jq-clone '[1,2,3][]'