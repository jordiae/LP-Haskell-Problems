{-
eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql [x] [y] = False
eql [x] [] = False
eql [] [x] = False
eql (x:xs) (y:ys)
	| x /= y = False
	| otherwise = eql xs ys
-}

eql :: [Int] -> [Int] -> Bool
eql l1 l2 = length l1 == length l2 && all (==True) (zipWith (==) l1 l2)

{-
prod :: [Int] -> Int
prod [] = 1
prod [x] = x
prod (x:xs) = x * prod(xs)
-}
prod :: [Int] -> Int
prod list = foldr (*) 1 list
-- or foldl (*) 1 list

{-
prodOfEvens :: [Int] -> Int
prodOfEvens [] = 1
prodOfEvens (x:xs)
	| even x = x * prodOfEvens (xs)
	| otherwise = prodOfEvens (xs)
-}

prodOfEvens :: [Int] -> Int
prodOfEvens list = prod $ filter even list

-- generates the list of all the powers of 2
powersOf2 :: [Int]
powersOf2 =  [2^x | x <-[0..]]

{-
scalarProduct::[Float]->[Float]->Float
scalarProduct [] [] = 0
scalarProduct (x:xs) (y:ys) = x*y + scalarProduct (xs) (ys)
-}


scalarProduct :: [Float] -> [Float] -> Float
scalarProduct l1 l2 = foldl (+) 0 (zipWith (*) l1 l2)
