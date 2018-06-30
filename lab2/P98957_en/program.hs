ones :: [Integer]
ones = 1:ones

nats :: [Integer]
--nats = [0..]
nats = iterate (+1) 0

merge l [] = l
merge [] g = g
merge (x:xs) (y:ys) = x : y : merge xs ys

ints :: [Integer]
ints = 0: [x |  x <- merge [1..] [-1,-2..]]


triangulars :: [Integer]
triangulars = 0 : (scanl (+) 1 $ iterate (+1) 2)


factorials :: [Integer]
factorials = scanl (*) 1 $ iterate (+1) 1

fibs :: [Integer]
fibs = scanl (+) 0 (1:fibs)

{-
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors n 2)

hasDivisors :: Integer -> Integer -> Bool
hasDivisors n c
    | c*c > n = False
    | mod n c == 0 = True
    | otherwise = hasDivisors n (c+1)

primes :: [Integer]
primes = [x | x <- [1..],isPrime x]
-}

primes :: [Integer]
primes = lprimes $ iterate (+1) 2
  where lprimes (x:xs) = x:(lprimes $ filter (\y -> (mod y x)/=0) xs)


-- Generate the ordered sequence of the Hamming numbers: [1,2,3,4,5,6,8,9,…]. The Hamming numbers are those that only have 2, 3 and 5 as prime divisors.
hammings :: [Integer]
hammings = 1 : map (2*) hammings `merge` map (3*) hammings `merge` map (5*) hammings



{-
say :: Integer -> Integer
-- Generate the look-and-say sequence: [1,11,21,1211,111221,312211,13112221,1113213211,…].
lookNsay :: [Integer]
lookNsay = 1 : map say lookNsay
-}




nextRow :: [Integer] -> [Integer]
nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
 
-- tartaglia triangle (binomial coefficients)
tartaglia :: [[Integer]]
tartaglia = iterate nextRow [1]
 