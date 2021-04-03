module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.JParser

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

-- Parenthesis/grouping e.g. (. + 2) * 5
parseParenthesis :: Parser Filter
parseParenthesis = token $ char '(' *> parseFilter <* char ')'

-- Object indexing e.g. .field or Optional Object indexing e.g. .field?
parseObjectIndex :: Parser Filter
parseObjectIndex = token $ (OptObjIdx <$> (indexstring <* space <* char '?')) <|> (ObjIdx <$> indexstring)
  where
    indexstring = space *> char '.' *> some alphanum

-- Optional Generic object indexing e.g. .["field"]? or Generic object indexing e.g. .["field"]
parseGenericObjectIndex :: Parser Filter
parseGenericObjectIndex = token $ (OptGenericObjIdx <$> (indices <* space <* char '?')) <|> (GenericObjIdx <$> indices)
  where
    indexstring = space *> char '"' *> (some jChar <|> pure []) <* char '"' <* space
    indices = char '.' *> space *> char '[' *>  ((:) <$>  indexstring <*> many (char ',' *> indexstring)) <* space <* char ']'

parseArrayIndex :: Parser Filter
parseArrayIndex = token $ (OptArrIdx <$> (index <* space <* char '?')) <|> (ArrIdx <$> index)
  where
    index = (char '.' *> space *> char '[' *> space *> integer <* space <* char ']')

parseSlicer :: Parser Filter
parseSlicer = token $ optSlice <|> optSliceAlt <|> normalSlice <|> normalSliceAlt
  where
    upperbound = pure (maxBound `div` 2)
    normalSlice = char '.' *> space *> char '[' *> space *>  (Slicer <$> (integer <|> pure 0) <*> (space *> char ':' *> space *> integer)) <* space <* char ']'  -- handles [0:1] and [0:] type of slices
    normalSliceAlt = char '.' *> space *> char '[' *> space *>  (Slicer <$> integer <*> (space *> char ':' *> space *> upperbound)) <* space <* char ']'         -- handles [:1] type of slices
    optSlice = char '.' *> space *> char '[' *> space *>  (OptSlicer <$> (integer <|> pure 0) <*> (space *> char ':' *> space *> integer)) <* space <* char ']' <* space <* char '?'
    optSliceAlt = char '.' *> space *> char '[' *> space *>  (OptSlicer <$> integer <*> (space *> char ':' *> space *> upperbound)) <* space <* char ']' <* space <* char '?'

parseIterator :: Parser Filter
parseIterator = token $ (OptIterator <$> iter <* char '?') <|> (Iterator <$> iter)
  where
    number = space *> integer <* space
    iter = char '.' *> space *> char '[' *> (((:) <$> number <*> many (char ',' *> number)) <|> pure []) <* space <* char ']'

parseComma :: Parser Filter
parseComma = space *> (CommaOperator <$> parseOthers <*> (space *> char ',' *> space *> parseFilter)) <* space

parsePipe :: Parser Filter
parsePipe = space *> (PipeOperator <$> parseOthers <*> (space *> char '|' *> space *> parseFilter)) <* space

-- Version without the parseComma or parsePipe to avoid infinite recursion
parseOthers :: Parser Filter
parseOthers = parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity

parseFilter :: Parser Filter
parseFilter = parseComma <|> parsePipe <|> parseSlicer <|> parseGenericObjectIndex <|> parseObjectIndex <|> parseArrayIndex <|> parseIterator <|> parseParenthesis <|> parseIdentity

parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
