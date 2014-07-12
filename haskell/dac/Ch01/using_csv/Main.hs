import Text.CSV

main :: IO ()
main = do
  let fileName = "mcdonalds.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv

handleError csv = putStrLn "error parsing"

doWork csv = (print.getLongs) csv

getLongs :: [Record] -> [String]
getLong [] = []
getLong [line] = line !! 1
getLongs (line:lines) = (line !! 1) : getLongs lines

--doWork csv = (print.findNorthern.tail) csv
--
--findNorthern :: [Record] -> Record
--findNorthern [] = []
--findNorthern
