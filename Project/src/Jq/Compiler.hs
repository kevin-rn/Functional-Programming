module Jq.Compiler where

import Jq.Filters
import Jq.Json

type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]
-- Handles value constructors
compile (ValueNull) _ = Right [JNull]
compile (ValueBool bool) _ = Right [JBool bool]
compile (ValueNumber num frac expo) _ = Right [JNumber num frac expo]
compile (ValueString chars) _ = Right [JString chars]
compile (ValueArray arr) inp = case (getArrayElements arr inp) of
    Right elements -> Right [JArray elements]
    Left msg -> Left msg
compile (ValueObject []) _ = Right [JObject []]
compile (ValueObject obj) inp = case (checkCompileError convertFlatten) of      -- check if the converted list has errors
    Left msg -> Left msg
    _ -> let tuples = (jsonList convertFlatten) in case checkAllKeys tuples of     -- Check if the keys are strings
        Left msg -> Left msg
        _ -> Right [JObject (removeDuplicateKeys keyvalue []) | keyvalue <- getKeyValuePairs (convertAllKeys tuples) ] -- Return list of objects with all possible unique key-value pairs
    where
        -- Creates list of [(Either String [JSON], Either String [JSON])]
        compiledKeyValues = [(compile k inp, compile v inp) | (k,v) <- obj]
        -- Changes list from [(Either String [JSON], Either String [JSON])] to [Either String ([JSON], [JSON])]
        convertFlatten = (flattenCompiledKeyValues compiledKeyValues)
        -- changes list from [Either String ([JSON], [JSON])]to [([JSON], [JSON])]
        jsonList = map (\(Right x) -> x)

-- Handle Basic Filters
compile (Identity) inp = return [inp]
compile (Parenthesis obj) inp = compile obj inp
compile (ObjIdx idx) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj]
    JNumber{}   -> Left "Error: cannot iterate over integer"
    JBool{}     -> Left "Error: cannot iterate over boolean"
    JString{}   -> Left "Error: cannot iterate over string"
    JArray{}    -> Left "Error: cannot iterate over array"
    _ -> Right [JNull]
compile (OptObjIdx idx) inp = case inp of 
    JObject obj -> Right [getObjectValue idx obj]
    JNull -> Right [JNull]
    _ -> Right []
compile (GenericObjIdx idx) inp = case (compile idx inp) of
    Left msg -> Left msg
    Right indices -> case inp of 
        JObject obj -> (getGenObjectValue indices obj False)
        JNumber{}   -> Left "Error: cannot iterate over integer"
        JBool{}     -> Left "Error: cannot iterate over boolean"
        JString{}   -> Left "Error: cannot iterate over string"
        JArray{}    -> Left "Error: cannot iterate over array"
        JNull-> Right [JNull]
compile (OptGenericObjIdx idx) inp = case (compile idx inp) of
    Left msg -> Left msg
    Right indices -> case inp of 
        JObject obj -> (getGenObjectValue indices obj True)
        JNull -> Right [JNull]
        _ -> Right []
compile (ArrIdx idx) inp = case inp of
    JArray arr ->  Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Left "Error: Cannot index non-array"
compile (OptArrIdx idx) inp = case inp of
    JArray arr -> Right [getArrayValue (checkNegIdx idx (length arr)) arr]
    _ -> Right []
compile (Slicer start end) inp = case (compile start inp, compile end inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right first, Right second) -> case inp of
        JArray _ -> flattenJProgram [getSlice s e inp False | (s, e) <- getCombinations first second]
        JString _ -> flattenJProgram [getSlice s e inp False | (s, e) <- getCombinations first second]
        JNull -> Right ([JNull] ++ [JNull])
        _ -> Left "Error: Cannot slice non-array/non-object"
compile (OptSlicer start end) inp = case (compile start inp, compile end inp) of
    (Right first, Right second) -> case inp of
        JArray _ -> flattenJProgram [getSlice s e inp True | (s, e) <- getCombinations first second]
        JString _ -> flattenJProgram [getSlice s e inp True | (s, e) <- getCombinations first second]
        JNull -> Right [JNull]
        _ -> Right []
    (_, _) -> Right []
compile (Iterator idxs) inp = case idxs of
    [] -> case inp of
        JArray arr -> Right arr
        JObject obj -> Right (map snd obj) 
        JNull -> Right [JNull]
        _ -> Left "Error: Cannot iterate over non-array/non-object "
    _ -> case (compile (head idxs) inp) of 
        Left msg -> Left msg
        Right indices -> case inp of
            JArray arr -> getIterateValue indices arr False
            JObject _ -> Left "Error: cannot do partial iteration over object"
            JNull -> Right [JNull]
            _ -> Left "Error: Cannot iterate over non-array/non-object"
compile (OptIterator idxs) inp = case idxs of
    [] -> case inp of
        JArray arr -> Right arr
        JObject obj -> Right (map snd obj) 
        JNull -> Right [JNull]
        _ -> Right []
    _ -> case (compile (head idxs) inp) of 
        Left msg -> Left msg
        Right indices -> case inp of
            JArray arr -> getIterateValue indices arr False
            JNull -> Right [JNull]
            _ -> Right []
compile (CommaOperator commaList) inp = handleCommas commaList inp
compile (PipeOperator pipeList) inp = case compile (head pipeList) inp of
    Right result -> handlePipes (tail pipeList) result
    Left msg -> Left msg
-- Handle advanced Filters
compile (RecursiveDescent) inp = Right (getRecursiveDescent inp)
compile (Equal lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 == r2) | (r1, r2) <- getCombinations result1 result2]
compile (Unequal lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 /= r2) | (r1, r2) <- getCombinations result1 result2]

compile (LowerThen lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 < r2) | (r1, r2) <- getCombinations result1 result2]
compile (GreaterThen lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 > r2) | (r1, r2) <- getCombinations result1 result2]
compile (LowerEq lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 <= r2) | (r1, r2) <- getCombinations result1 result2]
compile (GreaterEq lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right result1, Right result2) -> Right [JBool (r1 >= r2) | (r1, r2) <- getCombinations result1 result2]
compile (IfElse{}) _ = Left "ifthenelse not implemented yet"
-- compile (IfElse cond lexpr rexpr) inp = 
compile (AndLogic{}) _ = Left "and logic not implemented yet"
-- compile (AndLogic lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
--     (Left msg, _) -> Left msg
--     (_, Left msg) -> Left msg
--     (Right result1, Right result2) -> Right [JBool (handleAnd r1 r2) | (r1, r2) <- getCombinations result1 result2]
compile (OrLogic{}) _ = Left "or logic not implemented yet"
-- compile (OrLogic lexpr rexpr) inp = case (compile lexpr inp, compile rexpr inp) of
--     (Left msg, _) -> Left msg
--     (_, Left msg) -> Left msg
--     (Right result1, Right result2) -> Right [JBool (r1 || r2) | (r1, r2) <- getCombinations result1 result2]
compile (NotLogic) inp = case inp of 
    JNull       -> Right [JBool True]
    JBool False -> Right [JBool True]
    _           -> Right [JBool False]

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j

-- Generic combiner of elements of first and second list to create cartesian product list.
getCombinations :: [a] -> [b] -> [(a, b)]
getCombinations xs ys = [(x,y) | x <- xs, y <- ys]

-- Combine compiled lists of key and values to single list
flattenCompiledKeyValues :: [(Either String [JSON], Either String [JSON])] -> [Either String ([JSON], [JSON])]
flattenCompiledKeyValues [] = []
flattenCompiledKeyValues ((Left msg1, Left msg2):_) = [Left (msg1 ++ msg2)]
flattenCompiledKeyValues ((Left msg, Right _):_) = [Left msg]
flattenCompiledKeyValues ((Right _, Left msg):_) = [Left msg]
flattenCompiledKeyValues ((Right result1, Right result2):xs) = [Right (result1, result2)] ++ flattenCompiledKeyValues xs

-- Check if a list contains Left
checkCompileError :: [Either String ([JSON], [JSON])] -> Either String [a]
checkCompileError [] = Right []
checkCompileError ((Left msg):_) = Left msg
checkCompileError ((Right _):xs) = checkCompileError xs

-- Check for every object if their key is valid
checkAllKeys :: [([JSON], [JSON])] -> Either String [a]
checkAllKeys [] = Right []
checkAllKeys ((key, _):xs) = case checkKeys key of
    Left msg -> Left msg
    _ -> checkAllKeys xs

-- Check for one object if their key is valid
checkKeys :: [JSON] -> Either String [a]
checkKeys [(JString _)] = Right []
checkKeys ((JString _):xs) = checkKeys xs
checkKeys _  = Left "Error: cannot index object using non-string key"

-- Change all keys in all objects into from JSON to strings
convertAllKeys :: [([JSON], [JSON])] -> [([String], [JSON])]
convertAllKeys [] = []
convertAllKeys ((key, value):xs) = (convertKeys key, value):convertAllKeys xs

-- Change from one object all JSON keys into String keys
convertKeys :: [JSON] -> [String]
convertKeys [JString key] = [key]
convertKeys ((JString key):xs) = [key] ++ convertKeys xs
convertKeys _ = []

-- Get cartesian product of all keys and values
getKeyValuePairs :: [([String], [JSON])] -> [[(String, JSON)]]
getKeyValuePairs [] = []
getKeyValuePairs [(keys, values)] = [[(k, v)] | k <- keys, v <- values]
getKeyValuePairs ((keys, values):xs) = [(k, v):rest | k <- keys, v <- values, rest <- getKeyValuePairs xs]

-- Remove duplicate keys from a list of key value pairs
removeDuplicateKeys :: [(String, JSON)] -> [String] -> [(String, JSON)]
removeDuplicateKeys [] _ = []
removeDuplicateKeys (tuple@(key, _):xs) known = if key `elem` known then removeDuplicateKeys xs known else tuple: removeDuplicateKeys xs (key:known)

-- Gets all elements of the array
getArrayElements :: [Filter]-> JSON -> Either String [JSON]
getArrayElements []     _   = Right []
getArrayElements (x:xs) inp = case (compile x inp) of
    Left msg -> Left msg
    Right result1 -> case (getArrayElements xs inp) of
        Left msg -> Left msg
        Right result2 -> Right (result1 ++ result2)

-- Converts string to floating point number
convertNumber :: String -> Float
convertNumber num = read num :: Float


-- Check if negative is negative, if so then offset the index with a input length.
-- Check second time if new index is positive and return else set it to 0.
checkNegIdx :: Int -> Int -> Int
checkNegIdx idx len = if idx < 0 then let newidx = len + idx in (if newidx < 0 then 0 else newidx) else idx


-- Get the value in the object at a certain index or return null.
-- Input:   Key string   Object (list of key-value pair)
getObjectValue :: String -> [(String, JSON)] -> JSON
getObjectValue _   [] = JNull
getObjectValue idx ((key, value):xs) = if key == idx then value else getObjectValue idx xs


-- Get the values in the object at certain indices
-- Input:   list of JSON types - Object (list of key-value pair) - Boolean to indicate optional '?'
getGenObjectValue :: [JSON]-> [(String, JSON)] -> Bool -> Either String [JSON]
getGenObjectValue []                 _   _      = Right []
getGenObjectValue [(JString key)]    obj _      = Right [getObjectValue key obj]
getGenObjectValue ((JString key):xs) obj bool   = case (getGenObjectValue xs obj bool) of
    Right results -> Right ([getObjectValue key obj] ++ results)
    Left msg -> Left msg
getGenObjectValue _                  _   True   = Right []
getGenObjectValue _                  _   False  = Left "Error: Cannot index object with non-string indexing"


-- Get the value in the array at a certain index or return null.
getArrayValue :: Int -> [JSON] -> JSON
getArrayValue _   [] = JNull
getArrayValue idx (x:xs) = if idx == 0 then x else getArrayValue (idx - 1) xs


-- Finds indices for which the first list is a sublist for the other list
-- Input:           sublist - mainlist
findIndexSubList :: [JSON] -> [JSON] -> [JSON]
findIndexSubList [] _ = []
findIndexSubList arr mainlist = let sublists = (splitList (length arr) mainlist) in [(JNumber idx [] 0) | (idx, sub) <- (zip [0..] sublists), arr == sub]
    where
        splitList n xs = let sublist = take n xs in if length sublist < n then [] else (sublist : splitList n (tail xs))


-- Gets all values from iterator
-- Input:   list of JSON types - Input - Boolean to indicate optional '?'
getIterateValue :: [JSON] -> [JSON] -> Bool-> Either String [JSON]
getIterateValue []                   input _ = Right input
getIterateValue [(idx@JNumber{})]    input _ = let newidx = round (convertNumber (show idx)) in Right [getArrayValue newidx input]
getIterateValue ((idx@JNumber{}):xs) input bool = case (getIterateValue xs input bool) of
    Left   msg   -> Left msg
    Right values -> let newidx = round (convertNumber (show idx)) in Right ([getArrayValue newidx input] ++ values)
getIterateValue [(JArray arr)]       input _ = let indices = findIndexSubList arr input in Right [JArray indices]
getIterateValue ((jarr@JArray{}):xs) input bool = case (getIterateValue [jarr] input bool) of
    Left msg -> Left msg
    Right value -> case (getIterateValue xs input bool) of
        Left  msg    -> Left msg
        Right values -> Right (value ++ values)
getIterateValue _                    _     True = Right []
getIterateValue _                    _     False = Left "Error: Cannot index array with non-integer indexing"


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
getSlice _         _        (JArray _)  False = Left "Error: Start and end indices of an array slice must be numbers"
getSlice _         _         _          False = Left "Error: Cannot slice non-string/non-array"


-- Takes a list of Either objects and flattens it to a single Either object containing a Right with all values or the first Left encountered
flattenJProgram :: [Either String [JSON]] -> Either String [JSON]
flattenJProgram []                  = Right []
flattenJProgram ((Left msg):_)      = Left msg
flattenJProgram ((Right result):xs) = case (flattenJProgram xs) of
    Left msg -> Left msg
    Right results -> Right (result ++ results)


-- Helper method for compiling Comma Operator
-- Input: List of filters - input JSON
handleCommas :: [Filter] -> JSON -> Either String [JSON]
handleCommas []                _     = Left "Error: Cannot parse improper comma format"
handleCommas [comma]           input = compile comma input
handleCommas (comma:commaList) input = case (compile comma input) of
    Left msg -> Left msg
    Right result -> case (handleCommas commaList input) of
        Left msg -> Left msg
        Right results -> Right (result ++ results)


-- Helper methods for compiling Pipe Operator
-- Input: List of filters - input JSON list
handlePipes :: [Filter] -> [JSON] -> Either String [JSON]
handlePipes []     _     = Left "Error: Cannot parse improper pipe format"
handlePipes [x]    input = handleInput (compile x) input
handlePipes (x:xs) input = case (handleInput (compile x) input) of
    Left msg -> Left msg
    Right result -> (handlePipes xs result)


-- Handles all the JSON in the input list and returns a single Either
-- Input: single filter - input JSON list
handleInput :: (JProgram [JSON]) -> [JSON] -> Either String [JSON]
handleInput _ []     = Left "Error: Cannot parse improper format"
handleInput f [x]    = f x
handleInput f (x:xs) = case (f x) of 
    Left msg -> Left msg
    Right result -> case (handleInput f xs) of 
        Left msg -> Left msg
        Right results -> Right (result ++ results)

getRecursiveDescent :: JSON -> [JSON]
getRecursiveDescent input@(JObject obj) = input : (concatMap (getRecursiveDescent . snd) obj)
getRecursiveDescent input@(JArray arr)  = input : (concatMap getRecursiveDescent arr)
getRecursiveDescent input               = [input]