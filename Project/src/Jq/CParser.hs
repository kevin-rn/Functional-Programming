module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Data.Char(digitToInt, isAlphaNum)
import Jq.JParser (jChar)

---------- Value Constructors ----------
parseValueNull :: Parser Filter
parseValueNull = do
        _ <- symbol "null"<* optional (token (char '?'))
        return ValueNull

parseValueBool :: Parser Filter
parseValueBool = token ((ValueBool True <$ string "true" <* optional (token (char '?'))) <|> (ValueBool False <$ string "false") <* optional (token (char '?'))) -- Match either true or false

parseValueNumber :: Parser Filter
parseValueNumber = token (ValueNumber <$> valueNum <*> valueFract <*> valueExp <* optional (token (char '?')))
    where
        valueNum = fromIntegral <$> integer                                                                 -- handles whole numers
        valueFract = char '.' *> some (digitToInt <$> digit) <|> pure []                                    -- handle the fraction part
        valueExp = (char 'E' <|> char 'e') *> optional (char '+') *> (fromIntegral <$> integer) <|> pure 0  -- handle the exponential part

parseValueString :: Parser Filter
parseValueString = token (ValueString <$> (char '"' *> many jChar <* char '"' <* optional (token (char '?'))))

parseValueArray :: Parser Filter
parseValueArray = token (ValueArray <$> (char '[' *> space *> (valueArr <|> pure [])) <* space <* char ']' <* optional (token (char '?')) )
    where
        valueArr = (:) <$> (parsePipe <|> parseOthers) <*> many (char ',' *> (parsePipe <|> parseOthers))                             -- concatenate one or multiple array elements

parseValueObject :: Parser Filter
parseValueObject = token (ValueObject <$> (token (char '{') *> valueTuple <* token (char '}') <* optional (token (char '?')) ))
    where
        normaltuple =  ((\ ~(ValueString chars) value -> (ValueString chars, value)) <$> token parseValueString <* char ':' <*> (parsePipe <|> parseOthers))
        alttuple = ((\key value -> (ValueString key, value)) <$> ((:) <$> (letter <|> char '_') <*> many ((alphanum <|> char '_'))) <*> (parsePipe <|> parseOthers))
        parenthesisTuple = ((,) <$> parseParenthesis <* char ':' <*> (parsePipe <|> parseOthers))
        keyTuple = ((\ chars -> (ValueString chars, ObjIdx chars)) <$> ((:) <$> (letter <|> char '_') <*> many ((alphanum <|> char '_'))))
        tuple = normaltuple <|> alttuple <|> parenthesisTuple <|> keyTuple
        valueTuple =  (:) <$> tuple <*> many (token (char ',') *> tuple) <|> pure []

parseValueConstructor :: Parser Filter
parseValueConstructor = parseValueNull <|> parseValueBool  <|> parseValueNumber <|> parseValueString <|> parseValueArray <|> parseValueObject


---------- Basic Filters ----------

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

-- Parenthesis/grouping e.g. (. + 2) * 5
parseParenthesis :: Parser Filter
parseParenthesis = token $ char '(' *> parseFilter <* char ')'

-- Object indexing e.g. .field or Optional Object indexing e.g. .field?
parseObjectIndex :: Parser Filter
parseObjectIndex = token $ (OptObjIdx <$> ((indexstring <|> alternativestring) <* some (token (char '?')))) <|> (ObjIdx <$> (indexstring <|> alternativestring))
  where
    indexstring = token (char '.') *> char '"' *> many jChar <* char '"'
    alternativestring = space *> char '.' *> ((:) <$> (letter <|> char '_') <*> many (sat (\x -> isAlphaNum x || x == '_')))

-- Optional Generic object indexing e.g. .["field"]? or Generic object indexing e.g. .["field"]
parseGenericObjectIndex :: Parser Filter
parseGenericObjectIndex = token $ (OptGenericObjIdx <$> (indices <* some (token (char '?')))) <|> (GenericObjIdx <$> indices)
  where
    parseGenObjIdx = parsePipe <|> parseComma <|> parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity 
                  <|> parseValueNull <|> parseValueBool <|> parseValueString <|> parseValueArray <|> parseValueObject
    indices = char '.' *> token (char '[') *> token (parseGenObjIdx <|> pure (ValueString "")) <* token (char ']')

parseArrayIndex :: Parser Filter
parseArrayIndex = token $ (OptArrIdx <$> (index <* some (token (char '?')))) <|> (ArrIdx <$> index)
  where
    index = (char '.' *> space *> char '[' *> token (integer) <* char ']')

parseSlicer :: Parser Filter
parseSlicer = token $ (optSlice <|> optSliceAlt <|> normalSlice <|> normalSliceAlt)
  where
    indices = (char '(' *> token parseFilter <* char ')') <|> token parseFilter
    normalSlice = (char '.') *> space *> (char '[') *>  (Slicer <$> (indices <|> pure ValueNull) <*> (token (char ':') *> indices)) <* token (char ']')  -- handles [0:1] and [0:] type of slices
    normalSliceAlt = (char '.') *> space *> (char '[') *> (Slicer <$> (indices) <*> (token (char ':') *> pure ValueNull)) <* token (char ']')         -- handles [:1] type of slices
    optSlice = (char '.') *> space *> (char '[') *>  (OptSlicer <$> (indices <|> pure ValueNull) <*> (token (char ':') *> indices)) <* token (char ']') <* some (token (char '?'))
    optSliceAlt = (char '.') *> space *> (char '[') *> (OptSlicer <$> (indices) <*> (token (char ':') *> pure ValueNull)) <* token (char ']') <* some (token (char '?'))

parseIterator :: Parser Filter
parseIterator = token $ (OptIterator <$> iter <* some (token (char '?'))) <|> (Iterator <$> iter)
  where
    parseIter = parsePipe <|> parseComma <|> parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity 
              <|> parseValueNull <|> parseValueBool <|> parseValueNumber <|> parseValueArray <|> parseValueObject
    iter = (char '.') *> token (char '[') *> (((:) <$> token (parseIter) <*> pure []) <|> pure []) <* token (char ']')

parseComma :: Parser Filter
parseComma = token $ (CommaOperator <$> ((:) <$> token (parseOthers) <*> some (char ',' *> token (parseOthers))))

---- Used in Pipe when occuring as second argument
parsePipeGenObjIdx :: Parser Filter
parsePipeGenObjIdx = token $ (OptGenericObjIdx <$> (indices <* token (char '?'))) <|> (GenericObjIdx <$> indices)
  where
    parseGenObjIdx = parsePipe <|> parseComma <|> parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity 
              <|> parseValueNull <|> parseValueBool <|> parseValueString <|> parseValueArray <|> parseValueObject
    indices = optional (char '.') *> token (char '[') *> token (parseGenObjIdx) <* token (char ']')

parsePipeArrIdx :: Parser Filter
parsePipeArrIdx = token $ (OptArrIdx <$> (index <* token (char '?'))) <|> (ArrIdx <$> index)
  where
    index = optional (char '.') *> space *> char '[' *> token (integer) <* char ']'

parsePipeSlicer :: Parser Filter
parsePipeSlicer = token $ (optSlice <|> optSliceAlt <|> normalSlice <|> normalSliceAlt)
  where
    indices = (char '(' *> token parseFilter <* char ')') <|> token parseFilter
    normalSlice = optional (char '.') *> space *> (char '[') *>  (Slicer <$> (indices <|> pure ValueNull) <*> (token (char ':') *> indices)) <* token (char ']')  -- handles [0:1] and [0:] type of slices
    normalSliceAlt = optional (char '.') *> space *> (char '[') *> (Slicer <$> (indices) <*> (token (char ':') *> pure ValueNull)) <* token (char ']')         -- handles [:1] type of slices
    optSlice = optional (char '.') *> space *> (char '[') *>  (OptSlicer <$> (indices <|> pure ValueNull) <*> (token (char ':') *> indices)) <* token (char ']') <* (char '?')
    optSliceAlt = optional (char '.') *> space *> (char '[') *> (OptSlicer <$> (indices) <*> (token (char ':') *> pure ValueNull)) <* token (char ']') <* (char '?')

parsePipeIterator :: Parser Filter
parsePipeIterator = token $ (OptIterator <$> iter <* token (char '?')) <|> (Iterator <$> iter)
  where
    iter = optional (char '.') *> token (char '[') *> (((:) <$> token (parseFilter) <*> pure []) <|> pure []) <* token (char ']')

-- Fix Pipe operator as it doesn't work fully
parsePipe :: Parser Filter
parsePipe = token $ (PipeOperator <$> ((:) <$> token (parseComma <|> parseOthers) <*> some (normalPipe <|> genericPipe)))
  where
    normalPipe =  (char '|') *> token (parseComma <|> parseOthers)
    genericPipe = (space *> parseComma <|> parseObjectIndex <|> parsePipeGenObjIdx <|> parsePipeArrIdx <|> parsePipeSlicer <|> parsePipeIterator)

-- Version without the parseComma or parsePipe to avoid infinite recursion
parseOthers :: Parser Filter
parseOthers = parseAdvancedFilters <|> parseSlicer <|> parseArrayIndex <|> parseIterator <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseParenthesis <|> parseIdentity <|> parseValueConstructor

---------- Advanced Filters ----------
parseRecursiveDescent :: Parser Filter
parseRecursiveDescent = do
        _ <- symbol ".."
        return RecursiveDescent

parseEquality :: Parser Filter
parseEquality = token $ (Equal <$> token parseAdvancedFiltersAlt <*> (symbol "==" *> token parseAdvancedFiltersAlt))
                  <|> (Unequal <$> token parseAdvancedFiltersAlt <*> (symbol "!=" *> token parseAdvancedFiltersAlt))

parseIfElse :: Parser Filter
parseIfElse = token $ (IfElse <$> (symbol "if" *> token parseAdvancedFiltersAlt) <*> (symbol "then" *> token parseAdvancedFiltersAlt) <*> (symbol "else" *> token parseAdvancedFiltersAlt <* symbol "end"))

parseComparison :: Parser Filter
parseComparison = token $ (LowerThen <$> token parseAdvancedFiltersAlt <*> (symbol "<" *> token parseAdvancedFiltersAlt)) 
                      <|> (GreaterThen <$> token parseAdvancedFiltersAlt <*> (symbol ">" *> token parseAdvancedFiltersAlt))
                      <|> (LowerEq     <$> token parseAdvancedFiltersAlt <*> (symbol "<=" *> token parseAdvancedFiltersAlt))
                      <|> (GreaterEq   <$> token parseAdvancedFiltersAlt <*> (symbol ">=" *> token parseAdvancedFiltersAlt))

parseLogicConnectives :: Parser Filter
parseLogicConnectives = token $ (AndLogic <$> token parseAdvancedFiltersAlt <*> (symbol "and" *> token parseAdvancedFiltersAlt)) 
                      <|> (OrLogic <$> token parseAdvancedFiltersAlt <*> (symbol "or" *> token parseAdvancedFiltersAlt))

parseNotLogic :: Parser Filter
parseNotLogic = do
        _ <- symbol "not"
        return NotLogic

parseAdvancedFilters :: Parser Filter
parseAdvancedFilters = parseRecursiveDescent <|> parseIfElse <|> parseEquality <|> parseComparison <|> parseLogicConnectives <|> parseNotLogic

parseAdvancedFiltersAlt :: Parser Filter
parseAdvancedFiltersAlt = parseSlicer <|> parseArrayIndex <|> parseIterator <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseParenthesis <|> parseIdentity <|> parseValueConstructor

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseComma <|> parseAdvancedFilters <|> parseSlicer <|> parseArrayIndex <|> parseIterator <|> parseGenericObjectIndex 
  <|> parseObjectIndex <|> parseParenthesis <|> parseIdentity <|> parseValueConstructor
--  <|> parseValuePipe <|> 
parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e