module Task1_1 where

{-
  Задание 1.1
  Необходимо реализовать все операции, описанные в данном файле
-}

import Todo(todo)
import Data.Char

data Oper = Plus | Minus | Mult
    deriving(Show, Eq)

data Term = IntConstant {intValue :: Int}
            | Variable{ varName :: String}
            | BinaryTerm{ lhv :: Term, op :: Oper, rhv :: Term}
            deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
infixl 6 |+|
(|+|) :: Term -> Term -> Term
(|+|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|+|) (Variable l) (Variable r) = Variable (l ++ r)
(|+|) _ _ = error "ERROR"

infixl 6 |-|
(|-|) :: Term -> Term -> Term
(|-|) (IntConstant l) (IntConstant r) = IntConstant (l - r)
(|-|) _ _ = error "Expected IntConstant"

infixl 7 |*|
(|*|) :: Term -> Term -> Term
(|*|) (IntConstant l) (IntConstant r) = IntConstant (l + r)
(|*|) _ _ = error "Expected IntConstant"

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacemet (BinaryTerm l op r)
    | (Variable varName) == l = BinaryTerm replacemet op r
    | (Variable varName) == r = BinaryTerm l op replacemet
    | otherwise = error "There is no such variable"
replaceVar varName replacemet (Variable v)
    | (Variable varName) == (Variable v) = replacemet
    | otherwise = error "There is no such variable"
replaceVar varName replacemet (IntConstant i)
    | (IntConstant (read varName :: Int)) == (IntConstant i) = replacemet
    | otherwise = error "There is no such variable"

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate (BinaryTerm l op r)
    | op == Plus = l |+| r
    | op == Minus = l |-| r
    | op == Mult = l |*| r
    | otherwise = error "no such operation"
