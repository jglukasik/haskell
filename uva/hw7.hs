-- Yikes, this is a mess! I'll be sure not to use haskell for any vector
-- math from here on out. 

-- UVA Haskell lecture 7 homework:
-- Find the point of intersect of two lines using a package from Hackage

import Data.Vec as Vec
import Data.Maybe

--Initialize the vectors and their origins
v1 = 1:.2:.() :: Vec2 Float
v1Start = 3:.4:.() :: Vec2 Float

v2 = 4:.1:.() :: Vec2 Float
v2Start = 2:.5:.() :: Vec2 Float

--Compose the matrices to find intersection
m1 = transpose (v1:.(-v2):.() :: Mat22 Float)
v3 = v2Start - v1Start

--To find vS, the scaling vector for the solutions, do
--m1*vS=v3
vS = fromJust (solve m1 v3)

--To solve, add scaled vector to initial position
--If correct, sol1 == sol2
sol1 = v1Start + Vec.map (*Vec.head vS) v1
sol2 = v2Start + Vec.map (*Vec.last vS) v2
