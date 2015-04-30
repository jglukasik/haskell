{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html
import System.Random

-- Main function. Saves html and data file for processing animation
main :: IO ()
main = do
  g <- newStdGen
  writeHtml "processing.min.js" 
            "square.pde" 
            "square demo" 
            "square.html" 
            $ square g

-- Scaling factor
s :: Proc_Float
s = 200

square :: StdGen -> ProcScript
square  g = animateFigure Nothing
                          Nothing
                          50
                          (Color 0 0 0 255)
                          (squaref g)

squaref :: StdGen -> Proc_Int -> Figure
squaref g t = FillColor (Color 130 20 200 150)
                        (Polygon t1)
           <> FillColor (Color 50 250 170 150) 
                        (Polygon t2)
           <> FillColor (Color 170 50 110 150) 
                        (Polygon t3)
           <> FillColor (Color 10 150 210 150) 
                        (Polygon t4)
   where t1 = [(s,s),p1,p2]
         t2 = [(-s,s),p3,p4]
         t3 = [(-s,-s),p5,p6]
         t4 = [(s,-s),p7,p8]
         p1 = (s, s*sin(f*t'))
         p2 = (s*cos(f*t'), s)
         p3 = (-s, s*cos(f*t'))
         p4 = (s*sin(f*t'), s)
         p5 = (-s, s*sin(f*t'))
         p6 = (s*cos(f*t'), -s)
         p7 = (s, s*cos(f*t'))
         p8 = (s*sin(f*t'), -s)
         t' = intToFloat t
         f = (1/13)
   

-- Map over tuple elements
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x,y) = (f x, f y)
--
-- Infinte stream of uniformly generated points
unif :: StdGen -> [Proc_Point]
unif g = makeP (randoms g)
  where makeP (x:y:r) = mapT fromFloat (x,y):makeP r
        makeP _ = []

