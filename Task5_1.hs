module Task5_1 where

import Todo(todo)

-- Структура двусвязного списка из лекции про ленивость

data DList a = DNil 
             | DCons { 
                left :: (DList a), 
                current :: a, 
                right :: (DList a) 
             }

instance (Show a) => Show (DList a) where
    show it = "[" ++ showBody it ++ "]"
              where showBody DNil = ""
                    showBody (DCons _ h DNil) = show h
                    showBody (DCons _ h t) = show h ++ ", " ++ showBody t

instance (Eq a) => Eq (DList a) where
    DNil == DNil = True
    (DCons _ h1 t1) == (DCons _ h2 t2) = h1 == h2 && t1 == t2
    _ == _ = False

list2dlist :: [a] -> DList a
list2dlist lst = list2dlist' DNil lst

list2dlist' :: DList a -> [a] -> DList a
list2dlist' _ [] = DNil
list2dlist' left (h: t) = 
    let rec = DCons left h (list2dlist' rec t)
    in rec


-- Реализуйте функции индексирования, вставки и удаления элементов
index :: DList a -> Int -> a
index lst i = index' lst i 0 where
  index'  DNil              _ _             = error "index :: Out of range"
  index' (DCons _ curr rhv) i s | s == i    = curr
                                | otherwise = index' rhv i (s + 1)

insertAt :: DList a -> Int -> a -> DList a
insertAt list i value = insertAt' list i value 0 where
  insertAt'  DNil                           0 val _                    = DCons DNil val DNil
  insertAt' (DCons DNil curr DNil)          i val _   
                                                    | i == 1 = let rec = DCons DNil curr (DCons rec val DNil)
                                                               in rec
                                                    | otherwise        = error "insertAt1 :: Out of range"
  insertAt' (DCons (DCons l c r) curr DNil) i val sum 
                                                    | i == sum + 1     = DCons (DCons l c r) curr (DCons r val DNil)
                                                    | i >  sum + 1     = error "\n insertAt2 :: Out of range" 
  insertAt' (DCons lhv curr rhv)            i val sum 
                                                    | i < 0            = error "insertAt3 :: Out of range"
                                                    | sum == i         = DCons lhv val rhv
                                                    | otherwise        = DCons lhv curr (insertAt' rhv i val (sum + 1))

removeAt :: DList a -> Int -> DList a
removeAt  DNil                          _           = DNil
removeAt (DCons lhv curr DNil)          i 
                                        | i == 0    = DNil
                                        | otherwise = DCons lhv curr DNil 
removeAt (DCons lhv curr (DCons l c r)) i 
                                        | i == 0    = DCons lhv c (removeAt r (i - 1))   
                                        | otherwise = DCons lhv curr (removeAt (DCons l c r) (i - 1))
