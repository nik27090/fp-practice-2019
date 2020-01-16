module Task3_1 where

{-
  Задание 3.1
  Числа Пеано: представление чисел на основе нуля и операций "+1" и "-1".
  Необходимо реализовать все классы типов, характерные для целых чисел.
-}

data WeirdPeanoNumber = Zero | Succ WeirdPeanoNumber | Pred WeirdPeanoNumber

-- Реализуйте все классы типов, которым должны отвечать целые числа
toWPN :: Int -> WeirdPeanoNumber
toWPN x 
    | x > 0     = Succ (toWPN (x - 1))
    | x < 0     = Pred (toWPN (x + 1))
    | otherwise = Zero

toInt :: WeirdPeanoNumber -> Int
toInt x = toInt' x 0 where
    toInt' Zero sum     = sum
    toInt' (Succ x) sum = toInt' x (sum + 1)
    toInt' (Pred x) sum = toInt' x (sum - 1)

instance Show WeirdPeanoNumber where
    show Zero     = "Zero"
    show (Succ x) = "Succ(" ++ show x ++ ")"
    show (Pred x) = "Pred(" ++ show x ++ ")"

instance Eq WeirdPeanoNumber where
    (/=) x y = not (toInt x == toInt y)
    (==) x y = not (toInt x /= toInt y)

instance Ord WeirdPeanoNumber where
    compare x y 
        | toInt x > toInt y = GT
        | toInt y < toInt y = LT
        | otherwise         = EQ
    (<) x y  = toInt x < toInt y
    (<=) x y = toInt x <= toInt y
    (>) x y  = toInt x > toInt y
    (>=) x y = toInt x >= toInt y
    max x y
        | toInt x >= toInt y = x
        | otherwise          = y
    min x y
        | toInt x <= toInt y = x
        | otherwise          = y

instance Num WeirdPeanoNumber where
    (+) Zero y = y
    (+) x Zero = x
    (+) (Succ x) (Succ y) = Succ $ Succ $ (+) x y
    (+) (Pred x) (Pred y) = Pred $ Pred $ (+) x y
    (+) (Pred x) (Succ y) = Pred $ Succ $ (+) x y
    (+) (Succ x) (Pred y) = Succ $ Pred $ (+) x y

    (-) Zero y = negate y
    (-) x Zero = x
    (-) (Succ x) (Succ y) = x - y
    (-) (Pred x) (Pred y) = Pred $ Pred $ x - y
    (-) (Succ x) (Pred y) = Succ $ Succ $ x - y
    (-) (Pred x) (Succ y) = Pred $ Pred $ x - y

    (*) Zero y = Zero
    (*) x Zero = Zero
    (*) (Succ x) (Succ y) = Succ Zero + x + y + (x * y)
    (*) (Pred x) (Pred y) = Succ Zero + negate (x + y) + (x * y)
    (*) (Succ x) (Pred y) = Pred Zero + negate x + y + (x * y)
    (*) (Pred x) (Succ y) = Pred Zero + x + negate y + (x * y)

    negate Zero     = Zero
    negate (Succ x) = Pred (negate x)
    negate (Pred x) = Succ (negate x)

    abs x 
        | x >= Zero = x
        | otherwise = negate x

    signum x
        | x > Zero  = 1
        | x < Zero  = -1
        | otherwise = 0

    fromInteger i 
        | i == 0    = Zero
        | i < 0     = Pred $ fromInteger $ i + 1
        | otherwise = Succ $ fromInteger $ i - 1

instance Enum WeirdPeanoNumber where
    toEnum = toWPN
    fromEnum = toInt

instance Real WeirdPeanoNumber where
    toRational x = toRational $ toInt x

instance Integral WeirdPeanoNumber where
    toInteger x = toInteger' x 0 where
        toInteger' Zero sum     = sum
        toInteger' (Succ x) sum = toInteger' x (sum + 1)
        toInteger' (Pred x) sum = toInteger' x (sum - 1)

    quotRem _ Zero = error "divide by zero"
    quotRem x y    = quotRem' Zero (abs x) (abs y) where
        quotRem' q n m 
            | n >= m    = quotRem' (Succ q) (n - m) m
            | otherwise = (signum x * signum y * q, signum x * n)

    divMod = quotRem
