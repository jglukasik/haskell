{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html
import System.Random

main :: IO ()
main = do
  g <- newStdGen
  writeHtml "processing.min.js" 
            "bubbles.pde" 
            "Bubbles demo" 
            "bubbles.html" 
            $ bubbles g

bubbles :: StdGen -> ProcScript
bubbles g = animateFigure Nothing Nothing 50 (Color 0 0 0 255) (bubblesf g)

radius :: Proc_Float
radius = 50

speed :: Proc_Float
speed = 20

scale :: Proc_Float
scale = 100

numCircles :: Int
numCircles = 30

--pToProc scale p = (convert $ fst p, convert $ snd p)

bubblesf :: StdGen -> Proc_Int -> Figure
bubblesf g n =
 let t = (sin $ (intToFloat $ n) / speed ) * radius
     ps = map convert (take numCircles . normals . unif $ g)
     --rs = map ((* (radius / 10)) . (+ t) . fst) 
     --         (take numCircles . normals . unif $ g)
     rs = replicate numCircles t
     convert p = (move $ fst p, move $ snd p)
     move x = (+) (-scale/2) $ (*) scale x
 in  FillColor (Color 100 255 240 50)
   $ LineColor (Color 255 255 255 255)
   $ mconcat 
   $ (map . uncurry $ Circle) (zip ps rs)

-- Infinte stream of uniformly generated points
unif :: StdGen -> [Proc_Point]
unif g = makeP (randoms g)
  where makeP (x:y:r) = (fromFloat x,fromFloat y):makeP r
        makeP _ = []

-- Infinte stream of normally generated points
normals :: [Proc_Point] -> [Proc_Point]
normals ((x,y):ps) = (r*cos(t), r*sin(t)) : normals ps
  where r = (-2*log x)**0.5
        t = 2*pi*y
normals _ = []


