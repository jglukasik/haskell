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
main = do 
  indexPages <- getIndexPages
  mapM_ sortIndex indexPages

getIndexPages :: IO [FilePath]
getIndexPages = do
  contents <- readFile "index_pages.wiki"
  return [ x | x <- (splitOn "\n" contents), x /= ""]

sortIndex :: FilePath -> IO ()
sortIndex index = do
  contents <- readFile index
  let links = [ x | x <- (splitOn "\n" contents), isLink x, x /= ""]
  times <- mapM (getModificationTime . linkToFile) links
  let sorted = sortBy (comparing (Down . snd)) (zip links times)
  let newContents = intercalate "\n" (map fst sorted)
  writeFile index newContents

isLink :: String -> Bool
isLink ('[':'[':_) = True
isLink _ = False

linkToFile :: String -> FilePath  
linkToFile (x:xs) = case x of
                      '[' -> linkToFile xs
                      ']' -> linkToFile xs
                      _   -> x : linkToFile xs
linkToFile _ = ".wiki"

