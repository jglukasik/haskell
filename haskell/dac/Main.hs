import Control.Exception as C (catch, SomeException)
import System.Environment


main :: IO ()
main = do
  fileName <- getLine
  input <- C.catch (readFile fileName)
    $ \err -> print (err::SomeException) >> return ""
  print (countWords input)


countWords :: String -> [Int]
countWords input = map (length.words) (lines input)


