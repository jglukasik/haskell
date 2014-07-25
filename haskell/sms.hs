-- Parse the xml from sms backup and restore android app

import Data.Map (Map, fromList, toList)
import Text.XML.HXT.Core

import System.Environment(getArgs)
import System.Console.GetOpt
import System.IO
import System.Exit

main :: IO ()
main = do
  argv <- getArgs
  (al, src, dst) <- cmdlineOpts argv 
  input <- readFile src 
  [rc] <- runX (application al src dst)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess


cmdlineOpts :: [String] -> IO (SysConfigList, String, String)
cmdlineOpts argv = return ([withValidate no], argv!!0, argv!!1)

application cfg src dst
  = configSysVars cfg 
    >>>
    readDocument [] src
    >>>
    processChildren (processIt `when` isElem)
    >>>
    writeDocument [] dst
    >>>
    getErrStatus

processIt :: IOSArrow XmlTree XmlTree
processIt
  = deep 
    ( isElem 
      >>>
      hasName "sms" 
      >>>
      getAttrValue "body" 
      >>>
      arr addSpacing
      >>>
      mkText
    )
  where
  addSpacing :: String -> String
  addSpacing s = "\n" ++ s
