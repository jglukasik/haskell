--UVA haskell lecture 8 homework:
--Write an IO program that reverses text  

module Main where

main = do
  fileName <- getLine
  fileContents <- readFile fileName
  writeFile "output.txt" $ reverseTheText fileContents

reverseTheText :: String -> String
reverseTheText input = unlines (map reverse $ lines input)
