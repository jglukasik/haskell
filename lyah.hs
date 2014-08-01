maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
--maximum' (x:xs)
--	| x > maxTail = x
--	| otherwise = maxTail
--	where maxTail = maximum' xs
maximum' (x:xs) = max x (maximum' xs)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
	| n <= 0 = []
	| otherwise = x:replicate' (n-1) x


take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x:xs) = x:take' (n-1) xs


reverse' :: [a] -> [a]
reverse' [] = []
-- Not quite, use the ++ operator to put two lists together, because : takes an element and a list, where ++ takes two lists (i think)
-- reverse' (x:xs) = (reverse' xs):x   
reverse' (x:xs) = reverse' xs ++ [x]


repeat' :: a -> [a]
repeat' x = x:repeat' x


zip' :: [a] -> [b] -> [(a,b)]
zip' []_ = []
zip' _[] = []
-- Note, this works too! (++) takes two lists, which i enforce by throwing []'s around (x,y)
--zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


--elem' :: (Eq a) => a -> [a] -> Bool --Note; remember the restriction on Eq! only they can be compared!
--elem' n [] = False
--elem' n (x:xs) 		-- remember the fucking parens!!!
--	| n == x = True
--	| otherwise = elem' n xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
	let smallerSorted = quicksort (filter (<= x) xs) --[a | a <- xs, a <= x]
	    biggerSorted = quicksort (filter (>x) xs) --[a | a <- xs, a > x]  
	    -- Woah, i had two tabs here and it didnt work, watch that spacing!
	in smallerSorted ++ [x] ++ biggerSorted


isMyName :: Char -> Bool
isMyName = ( `elem` "Joseph Gordon Lukasik" )


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys


flip' :: (a->b->c) -> (b->a->c)
-- Yuck, i dont really like this definition, the next one given is far better
--flip' f = g
--where g x y = f y x
flip' f y x = f x y


largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
	where p x = x `mod` 3829 == 0


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
	| odd n = n:chain(n*3+1)
	| even n = n:chain(n `div` 2)


numLongChains :: Int
--numLongChains = length( filter isLong (map chain [1..100]))
--	where isLong xs = length xs > 15
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl(\acc x -> if x == y then True else acc) False ys


-- use right folds to build a list, it is easier to prepend with : then append with ++
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> fx:acc) [] xs
