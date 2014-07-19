-- Parse the xml from sms backup and restore android app

import Text.XML.HXT.Core
import System.Environment(getArgs)

main :: IO ()
main = do
  args <- getArgs
  let fileName = case args of
       (a:_) -> a
       _ -> "input.xml"
  input <- readFile fileName
  xml <- runX $ readString [withValidate no] input
  print xml  



