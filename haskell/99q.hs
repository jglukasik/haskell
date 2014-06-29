-- Problem 1
findLast :: [a] -> a
--findLast (w:[]) = w
findLast [w] = w
findLast (w:ws) = findLast ws


-- Problem 2
findNextLast :: [a] -> a
--findNextLast (x:y:[]) = x
findNextLast [x,_] = x
findNextLast (_:xs) = findNextLast xs


-- Problem 3
--elementAt :: [a] -> (Num b) => b -> a
elementAt [] _ = error "Index out of bounds"
elementAt (x:xs) n
	| n < 1 = error "Index out of bounds"
	| n == 1 = x 
	| otherwise = elementAt xs (n-1)


-- Problem 4
--findNum :: (Inegral n) -> [a]
--findNum xs = foldl(\acc _ -> acc + 1) 0 xs
findNum = foldl(\acc _ -> acc + 1) 0


-- Problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- Problem 6
--isPalindrome :: [a] -> Bool
isPalindrome xs
	| f == l = True
	| otherwise = isPalindrome mid
	where f = head xs
	      l = last xs
	      mid = init(tail(xs))
--ehh wrong
