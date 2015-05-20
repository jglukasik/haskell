{-
 - My idea for this is a program to manage my wiki. It can shuffle around links
 - to make the most frequented pages easily accessible. Maybe do spell check,
 - auto commit stuff, give me a commit of the day prompt... we'll see
 -
 -}

import Data.List
import Data.Ord
import System.Directory

main :: IO()
main = do
  files <- getCurrentDirectory >>= getDirectoryContents
  times <- mapM getModificationTime files
  let sorted = sortBy (comparing snd) (zip files times)
  -- TODO: instead of `reverse`ing here, reverse sort ordering above
  mapM_ print $ reverse sorted
