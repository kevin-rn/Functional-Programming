module BoolExpr where
import Parsing

data Prop = Lit Bool           -- True or False
          | Var Char           -- 'x', 'y', 'z', ...
          | Not Prop           -- ~ p
          | And Prop Prop      -- p /\ q
          | Imply Prop Prop    -- p => q
  deriving (Show)

propP :: Parser Prop
propP = do
  p <- atomP
  (do
      symbol "/\\"
      q <- atomP
      return (And p q)
   <|>
   do
       symbol "=>"
       q <- atomP
       return (Imply p q)
   <|>
   return p)

atomP :: Parser Prop
atomP =
  do
      symbol "True"
      return (Lit True)
  <|>
  do
      symbol "False"
      return (Lit False)
  <|>
  do
      symbol "~"
      p <- propP
      return (Not p)
  <|>
  do
      symbol "("
      p <- propP
      symbol ")"
      return p
  <|>
  do
      c <- item 
      return (Var c)

main :: IO ()
main = do
    putStrLn "Enter a boolean expression"
    s <- getLine 
    case parse propP s of
        [(p,"") ] -> putStrLn ("Parsed expression: " ++ show p)
        [(_,out)] -> putStrLn ("Unused input: " ++ out)
        []        -> putStrLn "Parsing failed."