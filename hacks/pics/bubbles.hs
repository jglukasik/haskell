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
radius = 40

speed :: Proc_Float
speed = 15

scale :: Proc_Float
scale = 150

numCircles :: Int
numCircles = 120

bubblesf :: StdGen -> Proc_Int -> Figure
bubblesf g n = FillColor (Color 100 255 240 50) 
             $ LineColor (Color 255 255 255 255) 
             $ mconcat 
             $ zipWith FillColor 
                       (take numCircles . randColor $ g)
                       $ (map . uncurry $ Circle) (zip ps rs)
 where rs = map ( (* radius)
                . sin
                . (* (1/speed)) 
                . (+ intToFloat n) 
                . (* scale) 
                . fst 
                ) (take numCircles . normals . unif $ g)
       ps = map (mapT ( (+ (-scale/2) )
                      . (* scale) 
                      ) 
                ) (take numCircles . normals . unif . snd . next $ g)

-- Map over tuple elements
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x,y) = (f x, f y)

-- Infinite stream of random colors
randColor :: StdGen -> [Color]
randColor g = makeC (randoms g :: [Float])
  where makeC (r:b:g:xs) = Color (s r) (s b) (s g) 100 : makeC xs
        makeC _ = []
        s = fromInt . floor . (* 255)

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


