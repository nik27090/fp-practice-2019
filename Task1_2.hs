module Task1_2 where

{-
  Задание 1.2
  Необходимо реализовать четыре любые функции в данном файле
-}

import Todo(todo)

-- синус числа (формула Тейлора)
sin' :: Double -> Double -> Double -> Double -> Double -> Double
sin' x a n s eps | (abs a) <= eps = s
                 | otherwise      = sin' x ((-a)*x*x/((n+1)*(n+2))) (n+2) (s+a) eps  
                   
mySin x = sin' x x 1 0 0.000000001

-- косинус числа (формула Тейлора)
cos' :: Double -> Double -> Double -> Double -> Double -> Double
cos' x a n s eps | (abs a) <= eps = s
                 | otherwise      = cos' x (((-a)*x*x)/(n*(n-1))) (n+2) (s+a) eps  
                   
myCos x = cos' x 1 2 0 0.000000001

-- наибольший общий делитель двух чисел
myGcd :: Integer -> Integer -> Integer
myGcd x y | x >= y    = gcd' x y 0
          | otherwise = gcd' y x 0

gcd' :: Integer -> Integer -> Integer -> Integer
gcd' x y res | y == 0    = abs res
             | otherwise = gcd' y (x `mod` y) y


-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist _ 0 = False
doesSquareBetweenExist from to | from == to      = False
                               | sq from == from = True
                               | otherwise       = doesSquareBetweenExist (from + 1) to
                               where sq x = floor ((sqrt $ fromIntegral x :: Double) ** 2)

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y = todo

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = todo

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo
