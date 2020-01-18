module Task4_2 where

{-
  Задание 4.1
  Реализация монады над множеством из четырёх элементов.
  Реализуйте все требуемые классы типов и объясните, почему они реализованы именно так.
-}

data FourOf a = FourOf a a a a deriving(Show,Eq)

-- реализуйте классы `Functor`, `Applicative` и `Monad` для типа `FourOf`
-- таким образом, что
-- do { x <- FourOf 1 2 3 4; y <- FourOf 4 6 7 8; return $ x + y } === FourOf 5 8 10 12

-- Функтор должен соответствовать следующим законам:
-- 1) fmap id = id
-- 2) fmap (p . q) = (fmap p) . (fmap q)

-- Данный функтор разворачивает значения из контекста -> применяет к каждому значению функцию -> и вновь заворачивает значения в контекст.
instance Functor FourOf where
--	fmap :: (a -> b) -> f a -> f b
    fmap f (FourOf a b c d) = FourOf (f a) (f b) (f c) (f d)

-- У данного аппликативного функтора значение и функция обернута в контекст -> монада их распаковывает -> применяет функцию к значению ->
-- -> получаем новое значение в контексте
instance Applicative FourOf where
    pure a = FourOf a a a a
 --(<*>) :: f (a -> b) -> f a -> f b
    (<*>) (FourOf f1 f2 f3 f4) (FourOf a b c d) = FourOf (f1 a) (f2 b) (f3 c) (f4 d)

-- Даная монада применяют функцию, которая возвращает упакованное значение, к упакованному значению. 
-- Тк функция(второй аргумент) возращает значение в конетксте, мы должны вытащить только одно значение из четырех
-- и поставить на соответствующую позицию, после чего обернуть в контекст. 
instance Monad FourOf where
-- (>>=) :: m a -> (a -> m b) -> m b
   (>>=) (FourOf a b c d) f = let one (FourOf a _ _ _)   = a
                                  two (FourOf _ b _ _)   = b
                                  three (FourOf _ _ c _) = c
                                  four (FourOf _ _ _ d)  = d
                                in FourOf (one (f a)) (two (f b)) (three (f c)) (four (f d))
