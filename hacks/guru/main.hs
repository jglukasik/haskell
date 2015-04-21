import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.List as L

import Data.Digest.Pure.SHA 
import System.Environment
import Control.Monad

main = do
  [s] <- getArgs
  g <- BS.readFile s
  mapM_ (putStrLn . showDigest . sha1) (BS.lines g)
  
