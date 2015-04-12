import System.Random
import System.Environment
import Data.Map.Strict as Map

unif :: StdGen -> [Double]
unif g = randoms g

coords :: [Double] -> [(Double, Double)]
coords (x:y:r) = (x,y):coords r

normal :: (Double, Double) -> Double
normal (u1, u2) = (-2*log u1)**0.5*cos(2*pi*u2)

normals :: [(Double, Double)] -> [Double]
normals (x:r) = normal x : normals r

placeInBins :: [Double] -> Map Int Int  -> Map Int Int
placeInBins (p:points) bins = insertWith (+) n 1 $ placeInBins points bins
  where n = floor (p / binWidth) 
        binWidth = 0.2
placeInBins _ bins = fromList []

makeStars :: [Int] -> [[Char]]
makeStars (p:rest) = replicate p '*' : makeStars rest
makeStars _ = []

printStars starList = mapM_ putStrLn . makeStars $ starList

genNormals :: StdGen -> [Double]
genNormals g = normals . coords . unif $ g

main = do
  g <- getStdGen
  (n:_) <- getArgs
  let points = take (read n::Int) $ genNormals g
  printStars . elems $ placeInBins points (fromList [])

  
