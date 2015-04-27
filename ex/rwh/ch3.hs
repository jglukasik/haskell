-- Exercises from Chapter 3 of Real World Haskell
import Data.List

makePalindrome :: [a] -> [a]
makePalindrome (x:xs) = x : (makePalindrome xs) ++ [x]
makePalindrome [] = []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x:xs)
  | xs == []     = True
  | x == last xs = isPalindrome (init xs)
  | otherwise    = False
isPalindrome [] = True

metaSort :: [[a]] -> [[a]]
metaSort list = sortBy byLength list
  where byLength a b = length a `compare` length b

data Direction = LeftT | RightT | Straight
                 deriving (Eq, Show)

data Point = Point
             { x :: Double
             , y :: Double
             } deriving (Eq, Show)

pointsToTuple :: [Point] -> [(Double, Double)]
pointsToTuple ps = [ ((x p), (y p)) | p <- ps]

getDirection :: Point -> Point -> Point -> Direction
getDirection a b c | crossProductZ > 0  = LeftT
        | crossProductZ < 0  = RightT
        | crossProductZ == 0 = Straight
      where
        crossProductZ 
          = ( ((x b) - (x a))*((y c) - (y a)) ) 
          - ( ((y b) - (y a))*((x c) - (x a)) )

getDirections :: [Point] -> [Direction]
getDirections (a:b:c:xs) = (getDirection a b c) : getDirections (b:c:xs)
getDirections _ = []

getMinPoint :: [Point] -> Point
getMinPoint ps = head (sortBy byX ps)
  where byX p1 p2 
          | (x p1) > (x p2) = GT
          | (x p1) < (x p2) = LT
          | otherwise       = (y p1) `compare` (y p2)
      

getConvexPoints :: [Point] -> [Point]
getConvexPoints (a:b:c:xs)
        | dir == RightT = getConvexPoints (a:c:xs)
        | otherwise     = a : (getConvexPoints (b:c:xs) )
      where
        dir = getDirection a b c
getConvexPoints (a:b:[]) = [a,b]
getConvexPoints _ = []

sortByAngleWith :: Point -> [Point] -> [Point]
sortByAngleWith p ps = sortBy byAngle ps
  where byAngle p1 p2 = angleBetween p p1 `compare` angleBetween p p2
        angleBetween b pn = atan $ ((y pn) - (y b)) / ((x pn) - (x b))

grahamScan :: [Point] -> [Point]
grahamScan ps = getConvexPoints $ minP : sortByAngleWith minP rest
  where minP = getMinPoint ps
        rest = delete minP ps

grahamScan' :: [Point] -> [Point]
grahamScan' ps = minP : sortByAngleWith minP rest
  where minP = getMinPoint ps
        rest = delete minP ps

-- Example for testing
pTuples = [(0,4), (1,1), (2,2), (4,4), (0,0), (1,2), (3,1), (3,3)] 
          :: [(Double,Double)]
ps = [Point x y | (x,y) <- pTuples]
