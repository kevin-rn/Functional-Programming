module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Data.Char(digitToInt)

jChar :: Parser Char
jChar = '\b' <$ string "\\b"                           -- backspace
       <|> '\f' <$ string "\\f"                        -- feed forward
       <|> '\n' <$ string "\\n"                        -- newline
       <|> '\r' <$ string "\\r"                        -- carriage return
       <|> '\t' <$ string "\\t"                        -- tab
       <|> '\\' <$ string "\\\\"                       -- 
       <|> '"' <$ string "\\\""                        -- quotation mark
       <|> '/' <$ string "\\/"                         -- solidus
       <|> sat (\c -> not (c == '\"' || c == '\\'))

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = (JBool True <$ string "true") <|> (JBool False <$ string "false")                          -- Match either true or false

parseJNumber :: Parser JSON
parseJNumber = JNumber <$> jNum <*> jFract <*> jExp
    where
        jNum = fromIntegral <$> integer                                                                 -- handles whole numers
        jFract = char '.' *> some (digitToInt <$> digit) <|> pure []                                    -- handle the fraction part
        jExp = (char 'E' <|> char 'e') *> optional (char '+') *> (fromIntegral <$> integer) <|> pure 0  -- handle the exponential part

parseJString :: Parser JSON
parseJString = JString <$> (char '"' *> many jChar <* char '"')

parseJArray :: Parser JSON
parseJArray = (JArray <$> (char '[' *> space *> (jArr <|> pure [])) <* space <* char ']')
    where
        jArr = (:) <$> parseJSON <*> many (char ',' *> parseJSON)                                         -- concatenate one or multiple array elements

parseJObject :: Parser JSON
parseJObject = (JObject <$> (char '{' *> space *> (jObj <|> pure [])) <* space <* char '}')
    where
        maptuple = (\ ~(JString key) value -> (key, value))                             -- map values of the string key and json value to a tuple
        keystr = space *> parseJString <* space                                         -- remove spaces surrounding the key string.
        jTuple =  maptuple <$> keystr <* char ':' <*> parseJSON                         -- chain them together
        jObj = (:) <$> jTuple <*> many (char ',' *> jTuple)                             -- concatenate one or multiple key value pairs

parseJSON :: Parser JSON
parseJSON = token $ parseJNull <|> parseJBool  <|> parseJNumber <|> parseJString <|> parseJArray <|> parseJObject