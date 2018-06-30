import Data.List -- per nub (treure duplicats)

myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs





myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
	| x > m 	= x
	| otherwise = m
	where m = myMaximum xs





totalSum :: [Int] -> Int
totalSum [x] = x
totalSum (x:xs) = x + totalSum(xs)

average :: [Int] -> Float
average xs = fromIntegral (totalSum xs) / fromIntegral (myLength xs)






reverseList :: [Int] -> [Int]
reverseList [last] = [last]
reverseList (head:tail) = (reverseList tail) ++ [head]

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome list = (reverseList list) ++ (list)




remove :: [Int] -> [Int] -> [Int]
remove l1 [] = l1
remove l1 (x:l2) = remove (remove' l1 x) l2
	where
		remove' :: [Int] -> Int -> [Int]
		remove' [] _ = []
		remove' (x:l) y
			| x == y	= remove' l y
			| otherwise = x:(remove' l y)






flatten :: [[Int]] -> [Int]
flatten [] = []
flatten [lastList] = lastList
flatten (head:tail) = head ++ flatten tail





oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
	| even x = (a,x:b)
	| otherwise = (x:a,b)
	where (a,b) = oddsNevens xs





primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
  | d == [] = [ x ]
  | otherwise = nub (d ++ primeDivisors(div x (head d)))
  where d = take 1 $ filter (\y -> (mod x y) == 0) [2 .. (x - 1)]