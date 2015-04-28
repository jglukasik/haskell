{-# LANGUAGE OverloadedStrings #-}

import GHC.Float
import Data.List
import System.Random

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html

data Direction = LeftT | RightT | Straight
                 deriving (Eq, Show)

type P = (Double, Double)

grahamScan :: [P] -> [P]
grahamScan ps = getConvexPs [minP] $ sortBy (cmpAngle minP) rest
  where minP = getMinP ps
        rest = delete minP ps

cmpAngle :: P -> P -> P -> Ordering
cmpAngle p x y  = angle p x `compare` angle p y
  where angle a b = atan $ ((snd b) - (snd a)) / ((fst b) - (fst a)) 

getDirection :: P -> P -> P -> Direction
getDirection a b c | crossProductZ > 0  = LeftT
        | crossProductZ < 0  = RightT
        | crossProductZ == 0 = Straight
      where
        crossProductZ 
          = ( ((fst b) - (fst a))*((snd c) - (snd a)) ) 
          - ( ((snd b) - (snd a))*((fst c) - (fst a)) )

getMinP :: [P] -> P
getMinP ps = head (sortBy byX ps)
  where byX p1 p2 
          | (fst p1) > (fst p2) = GT
          | (fst p1) < (fst p2) = LT
          | otherwise       = (snd p1) `compare` (snd p2)

getConvexPs :: [P] -> [P] -> [P]
getConvexPs (b:a:stack) (x:xs)
        | dir == RightT = getConvexPs (a:stack) (x:xs)
        | otherwise     = getConvexPs (x:b:a:stack) (xs)
      where
        dir = getDirection a b x
getConvexPs (a:[]) (x:xs) = getConvexPs [x,a] xs
getConvexPs [] (x:xs) = getConvexPs [x] xs
getConvexPs stack [] = stack

unif :: StdGen -> [P]
unif g = makeP (randoms g)
  where makeP (x:y:r) = (x,y):makeP r
        makeP _ = []

normals :: [P] -> [P]
normals ((x,y):ps) = (r*cos(t), r*sin(t)) : normals ps
  where r = (-2*log x)**0.5
        t = 2*pi*y
normals _ = []

main :: IO ()
main = do
  g <- newStdGen
  writeHtml "processing.min.js" 
            "graham.pde" 
            "Graham Scan" 
            "graham.html" 
            $ genScript g

genScript :: StdGen -> ProcScript
genScript g = 
  let n = 1000
      scale = 100
      r = 2
      
      s = (take n . normals . unif $ g)

      ps = map (pToProc scale) s
      f1 = map (FillColor (Color 255 255 255 255)) $
               (map . uncurry $ Circle) (zip ps (replicate n (fromFloat r)))

      cs = map (pToProc scale) (grahamScan s)
      f2 = map (FillColor (Color 255 0 0 100)) [Polygon cs]

  in displayFigure Nothing Nothing (Color 0 0 0 255) (mconcat (f1 ++ f2) )
            

pToProc :: Double -> P -> Proc_Point
pToProc scale p = (convert $ fst p, convert $ snd p)
  where convert x =  fromFloat . double2Float $ (x * scale) - scale/2
