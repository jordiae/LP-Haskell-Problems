-- Jordi Armengol

-- Apartat 1

multEq :: Int -> Int -> [Int]
multEq x y = iterate (*xy) 1
    where xy = x*y
          

-- Apartat 2

selectFirst :: [Int] -> [Int] -> [Int] -> [Int]
selectFirst l1 l2 l3 = [x | x <- l1, elem x l2, (not (elem x l3) || (index x l2 < index x l3))]
    where index element list = length ((\el l -> takeWhile (/= el) l) element list)
-- funcio auxiliar per obtenir l index d'un element suposant que ja sigui a la llista, no em sona que ja existeixi a prelude (similar, pero no el mateix: !!)
{-
index :: (Eq a) => a -> [a] -> Int
index x (y:ys) = index' x (y:ys) 0
    where
        index' x (y:ys) n
            | x == y = n
            | otherwise = index' x ys (n+1)
            
-}
            

-- Apartat 3
            
myIterate :: (a -> a) -> a -> [a]
myIterate f x =  x:(map ($x) (scanl (.) f (repeat f)))
          
-- Apartat 4
type SymTab a = String -> Maybe a

empty :: SymTab a
get :: SymTab a -> String -> Maybe a
set :: SymTab a -> String -> a -> SymTab a

empty = (\x -> Nothing)
get symtab string = symtab string
set symtab string a = \x -> if x == string then (Just a) else (symtab x)


-- Apartat 5

data Expr a
     = Val a
     | Var String
     | Sum (Expr a) (Expr a)
     | Sub (Expr a) (Expr a)
     | Mul (Expr a) (Expr a)
     deriving Show

eval :: (Num a) => SymTab a -> Expr a -> Maybe a 
eval _ (Val a) = Just a
eval symtab (Var s) = symtab s

eval symtab (Sum (e1) (e2)) = if (isNothing v1 || isNothing v2) then Nothing else Just (fromMaybe v1 + fromMaybe v2) 
    where
        v1 = eval symtab e1
        v2 = eval symtab e2
        
eval symtab (Sub (e1) (e2)) = if (isNothing v1 || isNothing v2) then Nothing else Just (fromMaybe v1 - fromMaybe v2) 
    where
        v1 = eval symtab e1
        v2 = eval symtab e2
        
eval symtab (Mul (e1) (e2)) = if (isNothing v1 || isNothing v2) then Nothing else Just (fromMaybe v1 * fromMaybe v2) 
    where
        v1 = eval symtab e1
        v2 = eval symtab e2

        
-- Nota: soc conscient que aquestes dues funcions estan implementades
-- a Data.Maybe, pero nomes podem fer servir Prelude
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x
