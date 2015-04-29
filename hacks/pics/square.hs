{-# LANGUAGE OverloadedStrings #-}

import Graphics.Web.Processing.Simple
import Graphics.Web.Processing.Html
import System.Random

-- Main function. Saves html and data file for processing animation
main :: IO ()
main = do
  writeHtml "processing.min.js" 
            "square.pde" 
            "square demo" 
            "square.html" 
            $ square

-- Scaling factor
s :: Proc_Float
s = 300

square :: ProcScript
square = animateFigure Nothing
                       Nothing
                       50
                       (Color 0 0 0 255)
                       squaref

squaref :: Proc_Int -> Figure
squaref t = Polygon triangle
   where triangle = [(s,s),(-s,s),(s,-s)]

-- Map over tuple elements
mapT :: (a -> b) -> (a, a) -> (b, b)
mapT f (x,y) = (f x, f y)
