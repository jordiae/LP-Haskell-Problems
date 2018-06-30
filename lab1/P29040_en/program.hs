insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert xxs@(x : xs) v
	| x > v = v : xxs
	| otherwise = x : insert xs v

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x





remove :: [Int] -> Int -> [Int]
remove [] _ = []
remove (x:xs) v
	| x == v = xs
	| otherwise = x : remove xs v

ssort :: [Int] -> [Int]
ssort [] = []
ssort l = [x] ++ ssort (remove l x)
	where x = minimum l




merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge a [] = a
merge [] b = b
merge a@(ax : axs) b@(bx : bxs)
	| ax < bx = ax : merge axs b
	| otherwise = bx : merge a bxs

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort l = merge (msort a) (msort b)
  where (a,b) = (take n l, drop n l)
        n = (div (length l) 2)





qsort :: [Int] -> [Int]
qsort [] = []
qsort (x : xs) = (qsort a) ++ [x] ++ (qsort b)
  where (a,b) = (filter (<=x) xs, filter (>x) xs)



genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x : xs) = (genQsort a) ++ [x] ++ (genQsort b)
  where (a,b) = (filter (<=x) xs, filter (>x) xs)