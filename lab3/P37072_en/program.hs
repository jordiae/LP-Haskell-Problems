data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ t1 t2) = 1 + size t1 + size t2


height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2) = 1 + max (height t1) (height t2)


equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal Empty (Node _ _ _) = False
equal (Node a t1 t2) (Node b t3 t4)
	| a /= b 	= False
	| otherwise = equal t1 t3 && equal t2 t4



isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic (Node a t1 t2) (Node b t3 t4)
	| a == b && equal t1 t3 && equal t2 t4 = True
	| a == b && equal t1 t4 && equal t2 t3 = True
	| otherwise = False



preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a t1 t2) = [a] ++ preOrder t1 ++ preOrder t2



postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a t1 t2) = postOrder t1 ++ postOrder t2 ++ [a] 



inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a t1 t2) = inOrder t1 ++ [a] ++ inOrder t2





breadthFirst :: Tree a -> [a]
breadthFirst (Node a t1 t2) = bfs [(Node a t1 t2)]
	where
		bfs :: [Tree a] -> [a]
		bfs [] = []
		bfs (Empty:ts) = bfs ts
		bfs ((Node a t1 t2):ts) = [a] ++ bfs (ts++[t1,t2])





-- x: pre-order traversal, y: in-order traversal
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:preOrderList) inOrderList = Node x (build leftPreOrderList leftInOrderList) (build rightPreOrderList rightInOrderList)
    where  
        leftInOrderList   = takeWhile (/= x) inOrderList
        leftPreOrderList  = take (length leftInOrderList) preOrderList
        rightPreOrderList = drop (length leftInOrderList) preOrderList
        rightInOrderList  = tail (dropWhile (/= x) inOrderList)


overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ (Node a t1 t2) Empty = (Node a t1 t2)
overlap _ Empty (Node b t3 t4) = (Node b t3 t4)
overlap f (Node a t1 t2) (Node b t3 t4) = (Node (f a b) (overlap f t1 t3) (overlap f t2 t4)) 

