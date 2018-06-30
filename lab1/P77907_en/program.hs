absValueCopiat :: Int -> Int
absValueCopiat = abs

absValue :: Int -> Int
absValue n =
	if n < 0 then n*(-1)
	else n

absValueAmbGuardes :: Int -> Int
absValueAmbGuardes n
	| n < 0 = n*(-1)
	| otherwise = n



{-
-- cost O(n)
power :: Int -> Int -> Int
power n p
	| p == 0 = 1
	| otherwise = n*power n (p-1)
-}

-- exponenciacio rapida O(log n) fastPower
power :: Int -> Int -> Int
power n p
	| p == 0 = 1
	| even p = y*y
	| otherwise = y*y*n
	where
		y = power n (div p 2)





isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = not (hasDivisors n 2)

hasDivisors :: Int -> Int -> Bool
hasDivisors n c
    | c*c > n = False
    | mod n c == 0 = True
    | otherwise = hasDivisors n (c+1)




slowFib :: Int -> Int
slowFib n
	| n == 0 = 0
	| n == 1 = 1
	| otherwise = slowFib (n-1) + slowFib (n-2)



quickFib :: Int -> Int
quickFib n = fst (quickFib2 n)


quickFib2 :: Int -> (Int,Int)
quickFib2 0 = (0,1)
quickFib2 n = (y, x+y)
	where (x,y) = quickFib2(n-1)


{-

quickFib n = qFib2 n 0 1
where
	qFib2 x y
	| n == 0 = x
	| otherwise = x+1



-- retorna el fibonacci de n i el de n+1 (per tenir finestra de 2)
quickFib2 :: Int -> (Int,Int)
quickFib2 0 = (0,1)
quickFib2 n = (y, x+y)
	where (x,y) = quickFib2(n-1)


-- tambe es pot escriure com a composicio de funcions
-- fst: first de pair
quickFib n = fst (quickFib2 n)
quickFib = fst  . quickFib2

-- comentaris de bloc: {- -}
-}