-- foldr equivalent to recursion. Better than foldl for flatten.

flatten :: [[Int]] ->  [Int]
flatten l = foldr (++) [] l

myLength :: String -> Int
myLength s = foldl (\x _ -> x+1) 0 s
-- myLength s = sum $ map (const 1)s

myReverse :: [Int] -> [Int]
myReverse l = foldl (flip (:)) [] l

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (length) $ map (filter (== x)) l

firstWord :: String -> String
firstWord s = takeWhile (/= ' ') $ dropWhile (== ' ') s 