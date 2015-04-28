{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html
import System.Random

-- Main function. Saves html and data file for processing animation
main :: IO ()
main = do
  g <- newStdGen  -- Random number generator
  let n = 50      -- Number of circles
  let r = 40.0    -- Radius
  let f = (1/15)  -- Frequency
  writeHtml "processing.min.js" 
            "bubbles.pde" 
            "Bubbles demo" 
            "bubbles.html" 
            $ bubbles g n f r

-- Driver function to create processing animation
bubbles :: StdGen     -- g Random number generator
        -> Int        -- n Number of circles
        -> Proc_Float -- f Frequency
        -> Proc_Float -- r Radius
        -> ProcScript
bubbles g n f r = animateFigure Nothing
                                Nothing
                                50
                                (Color 0 0 0 255)
                                (bubblesf g n f r)

-- Scaling factor to fill entire screen
scale :: Proc_Float
scale = 150

-- Function to create animation frames
bubblesf :: StdGen     -- g Random number generator
         -> Int        -- n Number of circles
         -> Proc_Float -- f Frequency
         -> Proc_Float -- r Radius
         -> Proc_Int   -- t Frame number
         -> Figure
bubblesf g n f r t = LineColor (Color 255 255 255 255)
             $ mconcat 
             $ zipWith FillColor 
                       ( take n . randColor $ g )
                       ( (map . uncurry $ Circle) (zip ps rs) )
 where rs = map ( (* r)
                . sin
                . (* f)
                . (+ intToFloat t)
                . (* scale) 
                . fst 
                ) (take n . normals . unif $ g)
       ps = map (mapT ( (+ (-scale/2) )
                      . (* scale) 
                      ) 
                ) (take n . normals . unif . snd . next $ g)

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
  where makeP (x:y:r) = mapT fromFloat (x,y):makeP r
        makeP _ = []

-- Transform uniform points to normal points using the box muller transform
normals :: [Proc_Point] -> [Proc_Point]
normals ((x,y):ps) = (r*cos(t), r*sin(t)) : normals ps
  where r = (-2*log x)**0.5
        t = 2*pi*y
normals _ = []


