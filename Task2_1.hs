module Task2_1 where

{-
  Задание 2.1
  На основе структуры бинарного дерева из лекции необходимо реализовать свою структуру данных
  бинарного дерева поиска (без балансировки) и все операции, приведённые в данном файле
-}

import Todo(todo)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = EmptyTree
               | Node (TreeMap v) (Integer, v) (TreeMap v)
               deriving (Show, Read, Eq)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = EmptyTree

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains EmptyTree _ = False
contains (Node t1 (key, _) t2) k 
    | key == k  = True
    | key > k   = contains t1 k
    | otherwise = contains t2 k 

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k t = todo

-- Значение для заданного ключа
lookup' :: TreeMap v -> Integer -> v
lookup' EmptyTree _ = error "lookop' :: Tree is empty"
lookup' (Node t1 (key, val) t2) k 
    | contains (Node t1 (key, val) t2) k = val
    | otherwise                          = error "lookop' :: Value does not exist"

-- Вставка пары (ключ, значение) в дерево
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, val) EmptyTree = Node EmptyTree (k, val) EmptyTree
insert (k2, v2) (Node t1 (k1, v1) t2)
    | k1 == k2  = Node t1 (k2, v2) t2 
    | k2 > k1   = Node t1 (k1, v1) (insert (k2, v2) t2)
    | otherwise = Node (insert (k2,v2) t1) (k1, v1) t2

-- Удаление элемента по ключу
remove ::  TreeMap v -> Integer -> TreeMap v
remove EmptyTree _ = EmptyTree
remove (Node t1 (key, val) t2) k 
    | k > key  = Node t1 (key, val) (remove t2 k)
    | k < key  = Node (remove t1 k) (key, val) t2
    | k == key =
--Если обоих детей нет, то удаляем текущий узел.
        if (isEmpty t1) && (isEmpty t2) 
            then EmptyTree
--Если удаляемый узел имеет только одного сына, заменем удаляемый узел на сына.
            else if (isEmpty t2)
                then t1
                else if (isEmpty t1)
                    then t2
--Если оба ребёнка присутствуют заменяем удаляемый узел элементом с наименьшим значением среди потомков правого сына
                    else if (True) 
                        then Node t1 (minRightTree t2) (remove t2 (minRightTreeVal $ minRightTree t2))
                        else error "remove :: should never work"

isEmpty :: TreeMap v -> Bool
isEmpty EmptyTree = True
isEmpty _ = False

minRightTree :: TreeMap v -> (Integer, v)
minRightTree t = head (listFromTree t) 

minRightTreeVal :: (Integer, v) -> Integer
minRightTreeVal (key, val) = key 

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i t = todo

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList lst = foldr insert EmptyTree lst

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree EmptyTree = []
listFromTree (Node t1 (key, val) t2) = listFromTree t1 ++ [(key, val)] ++ listFromTree t2 

-- Поиск k-той порядковой статистики дерева
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean i t = todo
