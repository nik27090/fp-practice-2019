module Task2_2 where

{-
  Задание 2.2
  Необходимо реализовать функции foldl, foldr и unfoldr, а также все остальные функции
  в данном файле _на основе этих трёх_
-}

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap,
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x0 [] = x0
foldl f x0 (x:xs) = foldl f (f x0 x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x0 [] = x0
foldr f x0 (x:xs) = f x (foldr f x0 xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of 
    Just (a, b') -> a : unfoldr f b'
    Nothing      -> []

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f = foldr (\ x xs -> f x : xs) []

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product = foldr (*) 1

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr f [] 
    where f Nothing lst  = lst
          f (Just i) lst = i : lst

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal [[]]       = []
diagonal (xs:[])    = [head xs]
diagonal (x:xs)     = head x : diagonal (map tail xs)

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot f = foldr (\ x xs -> if f x then xs else x : xs) []

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem i = foldr (\ x xs -> if x == i then True else xs) False 

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr (\b -> if b >= to then Nothing else Just (b, b+step)) from

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append start end = foldr (:) end start

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n = unfoldr (\x -> if null x then Nothing else Just (take (fromIntegral n) x, drop (fromIntegral n) x)) lst
