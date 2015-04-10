import System.Random

g1 = mkStdGen 1994
g2 = mkStdGen 2015

allPoints :: (Fractional a, Random a) => a -> [(a,a)]
allPoints r = zip (randomRs (0.0, r) g1) (randomRs (0.0, r) g2)

inCircle x = sum [ 1 | (a,b) <- x, (a**2 + b**2)**0.5 < 1.0]

--approxPi :: (Fractional a, Random a) => Int -> [(a,a)] -> a
approxPi n allPts = (fromIntegral $ inCircle $ take n $ allPts) / (fromIntegral n) * 4.0
