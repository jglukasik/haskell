import System.Random
import System.Environment
import Data.Map.Strict as Map
import Graphics.EasyPlot
import Control.Monad

-- TODO: 
--  - typeclasses, instead of choosing Double, StdGen
--  - non exhaustive pattern matching throughout, haven't ran into this issue
--    since i've only been dealing with infinite streams

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

normal :: Double -> Double -> StdGen -> [Double]
normal μ σ g = Prelude.map (+ σ * μ) (gaussian  g) 

normals :: [(Double, Double)] -> [Double]
normals ((x,y):r) = (-2*log x)**0.5*cos(2*pi*y) : normals r

placeInBins :: Double -> [Double] -> Map Int Int  -> Map Int Int
placeInBins width (p:rest) bins = insertWith (+) n 1 $ placeInBins width rest bins
  where n = floor (p / width) 
placeInBins width _ bins = fromList []

makeStars :: [Int] -> [[Char]]
makeStars (p:rest) = replicate p '*' : makeStars rest
makeStars _ = []

printStars starList = mapM_ putStrLn . makeStars $ starList

gaussian :: StdGen -> [Double]
gaussian g = normals . coords . unif $ g

main = do
  g <- getStdGen
  args <- getArgs
  let (opt:w:n:_) = args
  let points = take (read n::Int) $ gaussian g
  if 'a' `elem` opt
    then printStars . elems $ placeInBins (read w::Double) points (fromList [])
    else if 'g' `elem` opt
      then void . plot X11 $ Data2D [] [] $ coords points
      else putStrLn helpMessage
        
-- Tried plotting (X,Y) X ~ Unif, Y ~ Normal, but 
-- "take n $ zip (unif g2) (gaussian g2)" is O(n^2) i believe
  
