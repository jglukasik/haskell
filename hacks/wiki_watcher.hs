{-
 - My idea for this is a program to manage my wiki. It can shuffle around links
 - to make the most frequented pages easily accessible. Maybe do spell check,
 - auto commit stuff, give me a commit of the day prompt... we'll see
 -
 -}

import Data.List
import Data.List.Split
import Data.Ord
import System.Directory
import System.IO

main :: IO ()
main = sortIndex "index.wiki"

getIndexPages :: IO [FilePath]
getIndexPages = do
  contents <- readFile "index_pages.wiki"
  return (splitOn "\n" contents)

sortIndex :: FilePath -> IO ()
sortIndex index = do
  contents <- readFile index
  let links = filter (\x -> x /= "") $ splitOn "\n" contents
  times <- mapM (getModificationTime . linkToFile) links
  let sorted = sortBy (comparing (Down . snd)) (zip links times)
  let newContents = intercalate "\n" (map fst sorted)
  putStrLn newContents

linkToFile :: String -> FilePath  
linkToFile (x:xs) = case x of
                      '[' -> linkToFile xs
                      ']' -> linkToFile xs
                      _   -> x : linkToFile xs
linkToFile _ = ".wiki"

