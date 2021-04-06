module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Data.Char(digitToInt, isAlphaNum)

-- Handle value constructors
valueChar :: Parser Char
valueChar = '\b' <$ string "\\b"                       -- backspace
       <|> '\f' <$ string "\\f"                        -- feed forward
       <|> '\n' <$ string "\\n"                        -- newline
       <|> '\r' <$ string "\\r"                        -- carriage return
       <|> '\t' <$ string "\\t"                        -- tab
       <|> '\\' <$ string "\\\\"                       -- 
       <|> '"' <$ string "\\\""                        -- quotation mark
       <|> '/' <$ string "\\/"                         -- solidus
       <|> sat (\c -> not (c == '\"' || c == '\\'))

parseValueNull :: Parser Filter
parseValueNull = do
        _ <- token (string "null")
        return ValueNull

parseValueBool :: Parser Filter
parseValueBool = token ((ValueBool True <$ string "true") <|> (ValueBool False <$ string "false"))           -- Match either true or false

parseValueNumber :: Parser Filter
parseValueNumber = token (ValueNumber <$> valueNum <*> valueFract <*> valueExp)
    where
        valueNum = fromIntegral <$> integer                                                                 -- handles whole numers
        valueFract = char '.' *> some (digitToInt <$> digit) <|> pure []                                    -- handle the fraction part
        valueExp = (char 'E' <|> char 'e') *> optional (char '+') *> (fromIntegral <$> integer) <|> pure 0  -- handle the exponential part

parseValueString :: Parser Filter
parseValueString = token (ValueString <$> (char '"' *> many valueChar <* char '"'))

parseValueArray :: Parser Filter
parseValueArray = token (ValueArray <$> (char '[' *> space *> (valueArr <|> pure [])) <* space <* char ']')
    where
        valueArr = (:) <$> parseFilter <*> many (char ',' *> parseFilter)                             -- concatenate one or multiple array elements

parseValueObject :: Parser Filter
parseValueObject = token (ValueObject <$> (char '{' *> space *> (valueObj <|> pure [])) <* space <* char '}')
    where
        maptuple = (\ key value -> (key, value))                                     -- map values of the string key and json value to a tuple
        keystr = space *> parseValueString <* space                                                 -- remove spaces surrounding the key string.
        valueTuple =  maptuple <$> keystr <* char ':' <*> parseFilter                                -- chain them together
        valueObj = (:) <$> valueTuple <*> many (char ',' *> valueTuple)                             -- concatenate one or multiple key value pairs

parseValueConstructor :: Parser Filter
parseValueConstructor = parseValueNull <|> parseValueBool  <|> parseValueNumber <|> parseValueString <|> parseValueArray <|> parseValueObject

------- handle Filters -------------

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

-- Parenthesis/grouping e.g. (. + 2) * 5
parseParenthesis :: Parser Filter
parseParenthesis = token $ char '(' *> parseFilter <* char ')'

-- Object indexing e.g. .field or Optional Object indexing e.g. .field?
parseObjectIndex :: Parser Filter
parseObjectIndex = token $ (OptObjIdx <$> ((indexstring <|> alternativestring) <* token (char '?'))) <|> (ObjIdx <$> (indexstring <|> alternativestring))
  where
    indexstring = token (char '.') *> char '"' *> many valueChar <* char '"'
    alternativestring = space *> char '.' *> ((:) <$> (letter <|> char '_') <*> many (sat (\x -> isAlphaNum x || x == '_')))


-- Optional Generic object indexing e.g. .["field"]? or Generic object indexing e.g. .["field"]
parseGenericObjectIndex :: Parser Filter
parseGenericObjectIndex = token $ (OptGenericObjIdx <$> (indices <* token (char '?'))) <|> (GenericObjIdx <$> indices)
  where
    indexstring = space *> char '"' *> (some valueChar <|> pure []) <* char '"' <* space
    indices = char '.' *> space *> char '[' *>  ((:) <$>  indexstring <*> many (char ',' *> indexstring)) <* space <* char ']'

parseArrayIndex :: Parser Filter
parseArrayIndex = token $ (OptArrIdx <$> (index <* token (char '?'))) <|> (ArrIdx <$> index)
  where
    index = (char '.' *> space *> char '[' *> space *> integer <* space <* char ']')

parseSlicer :: Parser Filter
parseSlicer = token $ (optSlice <|> optSliceAlt <|> normalSlice <|> normalSliceAlt)
  where
    elements = ((:) <$> token (integer) <*> many (char ',' *> token (integer)))
    indices = char '(' *> elements <* char ')' <|> elements
    upperbound = pure [(maxBound `div` 2)]
    normalSlice = char '.' *> space *> char '[' *>  (Slicer <$> (indices <|> pure [0]) <*> (token (char ':') *> indices)) <* space <* char ']'  -- handles [0:1] and [0:] type of slices
    normalSliceAlt = char '.' *> space *> char '[' *> (Slicer <$> indices <*> (token (char ':') *> upperbound)) <* space <* char ']'         -- handles [:1] type of slices
    optSlice = char '.' *> space *> char '[' *>  (OptSlicer <$> (indices <|> pure [0]) <*> (token (char ':') *> indices)) <* space <* char ']' <* token (char '?')
    optSliceAlt = char '.' *> space *> char '[' *> (OptSlicer <$> indices <*> (token (char ':') *> upperbound)) <* space <* char ']' <* token (char '?')

parseIterator :: Parser Filter
parseIterator = token $ (OptIterator <$> iter <* token (char '?')) <|> (Iterator <$> iter)
  where
    iter = char '.' *> space *> char '[' *> ((:) <$> token (integer) <*> many (char ',' *> (token integer)) <|> pure []) <* space <* char ']'

parseComma :: Parser Filter
parseComma = token $ (CommaOperator <$> parseOthers <*> (token (char ',') *> (parseGenPipe <|> parseOthers)))
-- (parsePipe <|> parseOthers)

---- Used in Pipe when occuring as second argument
parsePipeGenObjIdx :: Parser Filter
parsePipeGenObjIdx = token $ (OptGenericObjIdx <$> (indices <* token (char '?'))) <|> (GenericObjIdx <$> indices)
  where
    indexstring = space *> char '"' *> (some valueChar <|> pure []) <* char '"' <* space
    indices = optional (char '.') *> space *> char '[' *>  ((:) <$>  indexstring <*> many (char ',' *> indexstring)) <* space <* char ']'

parsePipeArrIdx :: Parser Filter
parsePipeArrIdx = token $ (OptArrIdx <$> (index <* token (char '?'))) <|> (ArrIdx <$> index)
  where
    index = optional (char '.') *> space *> char '[' *> token (integer) <* char ']'

parsePipeSlicer :: Parser Filter
parsePipeSlicer = token $ optSlice <|> optSliceAlt <|> normalSlice <|> normalSliceAlt
  where
    elements = ((:) <$> (space *> integer <* space) <*> many (char ',' *> (space *> integer <* space)))
    indices = char '(' *> elements <* char ')' <|> elements
    upperbound = pure [(maxBound `div` 2)]
    normalSlice = optional (char '.') *> space *> char '[' *>  (Slicer <$> (indices <|> pure [0]) <*> (space *> char ':' *> space *> indices)) <* space <* char ']'  -- handles [0:1] and [0:] type of slices
    normalSliceAlt = optional (char '.') *> space *> char '[' *> (Slicer <$> indices <*> (space *> char ':' *> space *> upperbound)) <* space <* char ']'         -- handles [:1] type of slices
    optSlice = optional (char '.') *> space *> char '[' *>  (OptSlicer <$> (indices <|> pure [0]) <*> (space *> char ':' *> space *> indices)) <* space <* char ']' <* token (char '?')
    optSliceAlt = optional (char '.') *> space *> char '[' *> (OptSlicer <$> indices <*> (space *> char ':' *> space *> upperbound)) <* space <* char ']' <* token (char '?')

parsePipeIterator :: Parser Filter
parsePipeIterator = token $ (OptIterator <$> iter <* token (char '?')) <|> (Iterator <$> iter)
  where
    iter = optional (char '.') *> space *> char '[' *> (((:) <$> (space *> integer <* space) <*> many (char ',' *> token (integer))) <|> pure []) <* space <* char ']'

parsePipe :: Parser Filter
parsePipe = token $ (PipeOperator <$> (parseOthers) <*> (genericPipe <|> normalPipe))
  where
    normalPipe = (token (char '|') *> parseFilter)
    genericPipe = (space *> parseGenPipe)

parseGenPipe :: Parser Filter
parseGenPipe = token $ (PipeOperator <$> parseFirst <*> (space *> parseSecond))
  where
    parseFirst = parseComma <|> parseObjectIndex <|> parsePipeGenObjIdx <|> parsePipeArrIdx <|> parsePipeSlicer <|> parsePipeIterator
    parseSecond = parseGenPipe <|> parsePipe <|> parseFirst

parseValuePipe :: Parser Filter
parseValuePipe = token $ (PipeOperator <$> parseValueConstructor <*> parseSecond)
  where
    parseSecond = parseGenPipe <|> parsePipe <|> parseComma <|> parseObjectIndex <|> parsePipeGenObjIdx <|> parsePipeArrIdx <|> parsePipeSlicer <|> parsePipeIterator

-- Version without the parseComma or parsePipe to avoid infinite recursion
parseOthers :: Parser Filter
parseOthers = parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity <|> parseValueConstructor

parseFilter :: Parser Filter
parseFilter = parsePipe <|> parseValuePipe <|> parseComma <|> parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity <|> parseValueConstructor

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
