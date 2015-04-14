import System.Random
import System.Environment


unif :: StdGen -> [Double]
unif g = randoms g

coords :: [Double] -> [(Double, Double)]
coords (x:y:r) = (x,y):coords r

radius :: (Double, Double) -> Double
radius (x,y) = (x**2 + y**2)**0.5

inQuarterCircle :: [(Double, Double)] -> [Int]
inQuarterCircle coords = [if radius (x,y) < 1.0 then 1 else 0 
                          | (x,y) <- coords ]

approxPi :: Int -> StdGen -> Double
approxPi n g = fromIntegral numInCircle / fromIntegral n * 4.0
  where
    numInCircle = sum (take n . inQuarterCircle . coords . unif $ g) 

main = do
  g <- getStdGen
  (n:_) <- getArgs
  print $ approxPi (read n :: Int) g
