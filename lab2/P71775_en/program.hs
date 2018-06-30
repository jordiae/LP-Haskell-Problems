countIf :: (Int -> Bool) -> [Int] -> Int
-- countIf f l = foldr (\x y -> if f x then (y+1) else y) 0 l
countIf f a = length (filter f a)


-- map of maps...
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l f = map (`map` l) f


pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l f = map (\x -> map ($ x) f) l

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl p f i xs = foldl f i (filter p xs)




insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] 
insert r l x = takeWhile (not.r x) l ++ [x] ++ dropWhile (not.r x) l

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort _ [] = []
insertionSort p xs = foldl (insert p) [] xs