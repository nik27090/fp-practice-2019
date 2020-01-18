module Task6 where

{-
В этом файле приведён код, написанный (совместными усилиями) на лекции

Модифицируйте представленный тут парсер таким образом, чтобы он поддерживал унарные операции 
(отрицание и факториал), а также числа с плавающей точкой
-}

import Text.Parsec hiding(digit)
import Data.Functor

type Parser a = Parsec String () a

digit :: Parser Char
digit = oneOf ['0'..'9']

point :: Parser String
point = do 
    char '.'
    res <- many1 digit
    return ('.' : res)

number :: Parser Double
number =  do 
    lhv <- many1 digit 
    rhv <- option "" point 
    return $ read (lhv ++ rhv)

applyMany :: a -> [a -> a] -> a
applyMany x [] = x
applyMany x (h:t) = applyMany (h x) t

div_ :: Parser (Double -> Double -> Double)
div_ = do
    char '/'
    return (/)

star :: Parser (Double-> Double -> Double)
star = do
    char '*'
    return (*)

plus :: Parser (Double -> Double -> Double)
plus = do
    char '+'
    return (+)

minus :: Parser (Double -> Double -> Double)
minus = do
    char '-'
    return (-)

factorial :: Parser (Double -> Double)
factorial = do
    char '!'
    return fac

fac :: Double -> Double
fac n
    | n /= fromInteger (round n) = error "fac :: factorial of double"
    | n >= 0 = let
        helper acc 0 = acc
        helper acc n = helper (acc * n) (n - 1)
      in helper 1 n
    | otherwise = error "fac :: arg of factorial must be >= 0"

neg :: Double -> Double
neg x = x*(-1)

negative :: Parser (Double -> Double)
negative = do 
    char '-'
    return neg

negation :: Parser Double
negation = do 
    spaces
    t <- many negative
    spaces
    rhv <- atom
    return $ foldl (\x f -> f x) rhv t

factoriation :: Parser Double
factoriation = do 
    spaces
    t <- many factorial
    spaces
    rhv <- negation
    return $ foldl (\x f -> f x) rhv t    

multiplication :: Parser Double
multiplication = do
    spaces
    lhv <- factoriation
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- star <|> div_
                spaces
                rhv <- factoriation
                spaces
                return (`f` rhv)

addition :: Parser Double
addition = do
    spaces
    lhv <- multiplication
    spaces
    t <- many tail
    return $ applyMany lhv t
    where tail = 
            do
                f <- plus <|> minus
                spaces
                rhv <- multiplication
                spaces
                return (`f` rhv)

atom :: Parser Double
atom = number <|> do
    char '('
    res <- addition
    char ')'
    return res

start :: Parser Double
start = addition
