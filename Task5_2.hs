module Task5_2 where

import Todo(todo)

-- Зиппер из лекции 

data Zipper a = Zipper [a] [a]

-- Реализуйте экземпляры классов Show и Eq для этого типа

fromList :: [a] -> Zipper a
fromList lst = Zipper [] lst

goRight :: Zipper a -> Zipper a
goRight z@(Zipper _ []) = z
goRight (Zipper l (rh:rt)) = Zipper (rh:l) rt

goLeft :: Zipper a -> Zipper a
goLeft z@(Zipper [] _) = z
goLeft (Zipper (lh:lt) r) = Zipper lt (lh:r)

putRight :: a -> Zipper a -> Zipper a
putRight x (Zipper l r) = Zipper l (x:r)

putLeft :: a -> Zipper a -> Zipper a
putLeft x (Zipper l r) = Zipper (x:l) r

removeRight :: Zipper a -> Zipper a
removeRight (Zipper l (_:rt)) = Zipper l rt

removeLeft :: Zipper a -> Zipper a
removeLeft (Zipper (_:lt) r) = Zipper lt r

-- Используя приведённые выше функции, реализуйте функцию конкатенации
-- вставки подсписка в середину и выделения подсписка

instance (Show a) => Show (Zipper a) where
    show (Zipper lhv rhv) = "Zipper " ++ show lhv ++ " " ++ show rhv

instance (Eq a) => Eq (Zipper a) where
    (==) (Zipper lhv rhv) (Zipper l r) = (lhv ++ rhv) == (l ++ r)
    (/=) z1 z2 = z1 == z2      

concat :: Zipper a -> Zipper a -> Zipper a
concat (Zipper lhv rhv) (Zipper l r) = Zipper (lhv ++ rhv) (l ++ r)

toStart :: Zipper a -> Zipper a
toStart (Zipper l r) = case l of [] -> Zipper l r
                                 (_) -> toStart (goLeft (Zipper l r)) 

findIndex :: Zipper a -> Int -> Zipper a
findIndex zip i | i == 0    = zip 
                | otherwise = findIndex (goRight zip) (i - 1)

insertManyAt :: Int -> Zipper a -> Zipper a -> Zipper a
insertManyAt index what into = toStart (insertManyAt' (toStart what) (findIndex (toStart into) index)) where
    insertManyAt' (Zipper wleft wright) (Zipper inleft inright) = Zipper (inleft) (wright ++ inright)  

subZipper :: Int -> Int -> Zipper a -> Zipper a
subZipper from to input = subZipper' (findIndex (toStart input) from) (Zipper [] []) to where
    subZipper' (Zipper l r) (Zipper l2 r2) i 
        | i == 0 = Zipper l2 r2
        | otherwise = subZipper' (goRight (Zipper l r)) (putLeft (head r) (Zipper l2 r2)) (i - 1)
