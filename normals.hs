import System.Random
import System.Environment
import Data.Map.Strict as Map
import Control.Monad

helpMessage :: String
helpMessage = "Usage: ./normals BIN_WIDTH NUM_POINTS"

unif :: StdGen -> [Double]
unif g = randoms g

coords :: [Double] -> [(Double, Double)]
coords (x:y:r) = (x,y):coords r

normal :: (Double, Double) -> Double
normal (u1, u2) = (-2*log u1)**0.5*cos(2*pi*u2)

normals :: [(Double, Double)] -> [Double]
normals (x:r) = normal x : normals r

placeInBins :: Double -> [Double] -> Map Int Int  -> Map Int Int
placeInBins width (p:rest) bins = insertWith (+) n 1 $ placeInBins width rest bins
  where n = floor (p / width) 
placeInBins width _ bins = fromList []

makeStars :: [Int] -> [[Char]]
makeStars (p:rest) = replicate p '*' : makeStars rest
makeStars _ = []

printStars starList = mapM_ putStrLn . makeStars $ starList

genNormals :: StdGen -> [Double]
genNormals g = normals . coords . unif $ g

main = do
  g <- getStdGen
  args <- getArgs
  when (Prelude.null args || 'h' `elem` head args) (putStrLn helpMessage)
  let (w:n:_) = args
  let points = take (read n::Int) $ genNormals g
  printStars . elems $ placeInBins (read w::Double) points (fromList [])

  
