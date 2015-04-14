import System.Random
import System.Environment
import Data.Map.Strict as Map
import Graphics.EasyPlot
import Control.Monad

helpMessage :: String
helpMessage = "Usage: ./normals [-a|-g] BIN_WIDTH NUM_POINTS"

unif :: StdGen -> [Double]
unif g = randoms g

coords :: [Double] -> [(Double, Double)]
coords (x:y:r) = (x,y):coords r
coords _ = []

coords3 :: [Double] -> [(Double, Double, Double)]
coords3 (x:y:z:r) = (x,y,z):coords3 r
coords3 _ = []

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
  let (opt:w:n:_) = args
  let points = take (read n::Int) $ genNormals g
  if 'a' `elem` opt
    then printStars . elems $ placeInBins (read w::Double) points (fromList [])
    else if 'g' `elem` opt
      then void . plot X11 $ Data2D [] [] $ coords points
      else putStrLn helpMessage
        

  
