-- Parse the xml from sms backup and restore android app

import Text.XML.HXT.Core

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  argv <- getArgs
  (al, src, dst, attr) <- cmdlineOpts argv 
  input <- readFile src 
  [rc] <- runX (application al src dst attr)
  if rc >= c_err
    then exitWith (ExitFailure (0-1))
    else exitWith ExitSuccess


cmdlineOpts :: [String] -> IO (SysConfigList, String, String, String)
cmdlineOpts argv = return ([withValidate no], argv!!0, argv!!1, argv!!2)

application cfg src dst attr
  = configSysVars cfg 
    >>>
    readDocument [] src
    >>>
    processChildren (processIt attr `when` isElem)
    >>>
    writeDocument [] dst
    >>>
    getErrStatus

processIt :: String -> IOSArrow XmlTree XmlTree
processIt attr
  = deep 
    ( isElem 
      >>>
      hasName "sms" 
      >>>
      getAttrValue attr
      >>>
      arr addSpacing
      >>>
      mkText
    )
  where
  addSpacing :: String -> String
  addSpacing s = "\n" ++ s
