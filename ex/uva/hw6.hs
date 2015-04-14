import Data.Char


-- This seems really shitty... its taking four passes through the string!
-- I should do this all in one pass....
strong :: String -> Bool
strong pass = isLong pass
         && hasUpper pass
         && hasLower pass
         && hasDigit pass

isLong :: String -> Bool
isLong pass | length pass >= 15 = True
            | otherwise = False 

hasUpper :: String -> Bool
hasUpper pass = foldl (\acc x -> if isUpper x 
                                 then True
                                 else acc) False pass

hasLower :: String -> Bool
hasLower pass = foldl (\acc x -> if isLower x 
                                 then True
                                 else acc) False pass

hasDigit :: String -> Bool
hasDigit pass = foldl (\acc x -> if isDigit x 
                                 then True
                                 else acc) False pass

