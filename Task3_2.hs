module Task3_2 where

{-
  Задание 3.2
  Структура данных "перевёрнутый список" представляет собой зеркально обращённый
  односвязный список из стандартной библиотеки, для которого взятие последнего
  элемента является наиболее эффективной операцией.
  Необходимо реализовать все функции и классы типов в данном файле.
-}

import Todo(todo)

data ReverseList a = RNil | RCons (ReverseList a) a

rlistToList :: ReverseList a -> [a]
rlistToList RNil = []
rlistToList (RCons xs x) = reverse (x : rlistToList' xs) where
    rlistToList' (RCons RNil n) = [n]
    rlistToList' (RCons nm n)   = n : rlistToList' (nm)

listToRList :: [a] -> ReverseList a
listToRList lst = listToRList' (reverse lst) where
    listToRList' []     = RNil
    listToRList' (x:xs) = RCons (listToRList' xs) x 

-- Реализуйте классы Eq, Ord, Show, Monoid, Functor

instance (Show a) => Show (ReverseList a) where
    show RNil         = ""
    show (RCons xs x) = "[" ++ (show x) ++ "," ++ (show' xs) ++ "]" where
        show' RNil           = ""
        show' (RCons RNil n) = show n
        show' (RCons nm n)   = show n ++ "," ++ show' nm

instance (Eq a) => Eq (ReverseList a) where 
    (==) RNil RNil  = True
    (==) (RCons xs x) (RCons nm n)
        | x /= n    = False
        | otherwise = xs == nm
    (==) _ _        = False
    
    (/=) x y 
        | x == y    = False
        | otherwise = True

instance (Ord a) => Ord (ReverseList a) where
    compare RNil RNil = EQ
    compare RNil _    = LT
    compare _ RNil    = GT
    compare (RCons xs x) (RCons nm n) 
        | x > n     = GT
        | x < n     = LT
        | otherwise = compare xs nm 

instance Functor ReverseList where
    fmap _ RNil         = RNil
    fmap f (RCons xs x) = RCons (f <$> xs) (f x)

instance Monoid (ReverseList a) where
    mempty = RNil  

instance Semigroup (ReverseList a) where
    (<>) RNil RNil       = mempty
    (<>) a RNil          = a
    (<>) RNil b          = b
    (<>) xs (RCons nm n) = RCons (xs <> nm) n
