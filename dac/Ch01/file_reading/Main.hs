import Control.Exception as C (catch, SomeException)
import System.Directory(doesFileExist)
import System.Environment


main :: IO ()
main = do
  fileName <- getLine
  exists <- doesFileExist fileName
  input <- if exists then readFile fileName else return ""
  print (countWords input)


countWords :: String -> [Int]
countWords input = map (length.words) (lines input)


