

import Data.Char ( toUpper )

main :: IO ()
main = do
  input <- getContents
  putStr (map toUpper input)