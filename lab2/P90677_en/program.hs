myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x [] = x
myFoldl f x (a:as) = myFoldl f (f x a) as




myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x [] = x
myFoldr f x (a:as) = f a $ myFoldr f x as



myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)


myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil b f a = head (filter b (myIterate f a))



myMap :: (a -> b) -> [a] -> [b]
myMap f l = myFoldr (\x xs -> (f x):xs) [] l


-- myFilter
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = myFoldr (\x xs -> if f x then x:xs else xs) [] l



myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = and (myMap f l)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = or (myMap f l)


myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = myFoldr (\x xs -> (f (fst x) (snd x)):xs) [] $ myZip a b