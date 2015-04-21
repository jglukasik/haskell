import System.Random
import System.Environment

--TODO:
--  - With n > ~200000, we get a stack space overflow. It might be the case that
--  somewhere I'm not using tail recursion, forcing the program to keep a huge
--  list of numbers in the stack instead of getting rid of the head of the list
--  after we use it.

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
    numInCircle = n - (sumOutCircle . take n .  coords . unif $ g) 

main = do
  g <- getStdGen
  (n:_) <- getArgs
  print $ approxPi (read n :: Int) g


sumOutCircle :: [(Double, Double)] -> Int
sumOutCircle nums = foldl (\acc (x,y) -> acc + floor ((x**2 + y**2)**0.5) ) 0 nums

