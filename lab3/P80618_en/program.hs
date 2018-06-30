data Queue a = Queue [a] [a]
	deriving (Show)


create :: Queue a
create = Queue [] []


push :: a -> Queue a -> Queue a
push x (Queue l1 l2) = Queue l1 (x:l2) 



remove_first :: [a] -> [a]
remove_first [] = []
remove_first (x:xs) = (xs)

pop :: Queue a -> Queue a
pop (Queue [] ys) = Queue (remove_first $ reverse ys) []
pop (Queue (x:xs) ys) = Queue xs ys

top :: Queue a -> a
top (Queue (x:xs) _) = x
top (Queue [] (ys)) = last ys

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty (Queue _ _) = False


instance Eq a => Eq (Queue a)
     where
     	(Queue l1 l2) == (Queue l3 l4) = l12 == l34
     		where
     			l12 = l1 ++ reverse l2
     			l34 = l3 ++ reverse l4