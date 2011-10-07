-- Явно выпишем всё, что нам понадобится.
-- Мы об этих штуках ещё не говорили, потому
-- просто игнорируйте эти строчки.
-- В них мы из стандартной библиотеки хапаем
-- себе стандартные типы и функции. Целые числа
-- и операции их сравнения, например.
import Prelude ( Show(..)
               , Bool(..), Int(..), Double(..)
               , (+), (-), (*), (/), mod
               , (<), (==), (>), (<=), (>=))

---------------------------------------------
-- Синтаксис

-- Эквивалентные определения
example1 x  = x
example1'   = \x -> x
example1''  = let y = \x -> x in y
example1''' = y where
    y = \x -> x

-- Снова эквивалентные определения
example2 x y  = x + y
example2' x   = \y -> x + y
example2''    = \x -> \y -> x + y
example2'''   = \x y -> x + y
example2''''  = let z = \x y -> x + y in z
example2''''' = z where
    z x = \y -> x + y

-------------------------------------------
-- Примеры

otherwise = True

abs x | x < 0 = minusx
      | x == 0 = 0
      | otherwise = x
      where minusx = -x

-- Эквивалентны
gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b y
    where y = a `mod` b

gcd' a b = if b == 0
    then a
    else gcd' b (a `mod` b)
-- Обратите внимание, что gcd хвосторекурсивный.

-------------------------------------------
-- Операции над функциями

infixr 0 $
f $ x = f x

-- Эквивалентные определения
example3   = gcd 121 (gcd 44 11)
example3'  = gcd 121 $ gcd 44 11
example3'' = ($) (gcd 121) (gcd 44 11)

infixr 9 .
(.) f g = \x -> f (g x)

example4 x = (gcd 121 (gcd 44 x))
example4'  = gcd 121 . gcd 44

-------------------------------------------
-- Неподвижные точки
eps = 0.0001

infix 1 ~=
a ~= b = abs (a - b) < eps

average x y = (x + y) / 2
averageDamp f = \ x -> average x (f x)

fixedPoint f g = if g ~= f g
    then g
    else fixedPoint f (f g)

sqrt' x = fixedPoint (\y -> x/y) 1 -- Обычно не сходится.
sqrt x = fixedPoint (averageDamp (\y -> x/y)) 1 -- А этот работает.

-- Взятие производной: нам дали функцию, а мы вернули тоже взяли да
-- и вернули функцию.
deriv :: (Double -> Double) -> (Double -> Double)
deriv f x = (f (x + eps) - f x) / eps

-- Метод Ньютона ищет ноль функции.
newton f x = x - f x / deriv f x

-- Результаты первых двух будут немного отличаться,
sqrt'' x = fixedPoint (newton (\y -> y*y - x)) 1
sqrt''' x = fixedPoint (averageDamp $ newton (\y -> y*y - x)) 1
-- а эта и предыдущая одинаковые.
sqrt'''' x = fixedPoint (averageDamp . newton $ \y -> y*y - x) 1

-------------------------------------------
-- Классические рекурсивные определения

-- Обратите внимание, что вычисление этой
-- функции сначала <<расширяется>>, а потом
-- <<скукоживается>>:
-- fac 10 = 10 * fac 9
--        = 10 * 9 * fac 8
--        ...
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2 * 1
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
--        = 10 * 9 * 8 * 7 * 6 * 5 * 4 * 6
--        = 10 * 9 * 8 * 7 * 6 * 5 * 24
--        ...
--        = 3628800
fac 0 = 1
fac n = n * fac (n - 1)

-- а у этой версии ничего не разрастается:
-- fac' 10 = fac'' 10 1
--         = fac'' 9 10
--         = fac'' 9 90
--         ...
--         = fac'' 1 3628800
--         = fac'' 0 3628800
--         = 3628800
fac' n = fac'' n 1 where
    fac'' 0 x = x
    fac'' n a = fac'' (n - 1) (n*a)

-- Мне лень рисовать разрастание такого дерева.
-- Представьте сами.
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- И для этой представьте.
fib' n = fib'' n 1 1 where
    fib'' 0 p pp = pp
    fib'' n p pp = fib'' (n - 1) (pp + p) p

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Мы бы начали с булевых, но мы их уже использовали (оператор if).

-- Однако самостоятельно определить себе булев тип очень просто:
data Bool' = True' | False'
    deriving Show

-- После этого, например, можно определить свой оператор
-- if:
if' True  a b = a
if' False a b = b

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Начинаем с натуральных чисел (в математике -- жирная N)

data Nat = Zero | Succ Nat
--    deriving Show          -- (1)
instance Show Nat where  -- (2)
    show = show . toInt  -- (3)
-- Чёрная магия: если закомментировать строку 1 и раскомментировать 2 и 3,
-- то 
-- > Zero
-- интерпретатор напечатает как 0,
-- а
-- > Succ (Succ Zero)
-- интерпретатор напечатает как 2.

fromInt 0 = Zero                   -- Этот конструктор (Zero) -- просто значение
                                   -- типа Nat.
fromInt a = Succ $ fromInt (a - 1) -- А этот конструктор сильно напоминает функцию.

toInt Zero = 0
toInt (Succ a) = 1 + toInt a

-- Сложение (эквивалентные определения)
plus :: Nat -> Nat -> Nat
plus Zero b     = b
plus (Succ a) b = plus a (Succ b)

plus' a b = case a of
    Zero -> b
    (Succ a) -> plus' a (Succ b)

-- Другое сложение (ведёт себя иначе, но
-- результат такой же)
plus'' Zero b = Zero
plus'' (Succ a) b = Succ $ plus'' a b

-- * умножение (через сложение)
mul :: Nat -> Nat -> Nat
mul a b = mul' a b Zero

mul' :: Nat -> Nat -> Nat -> Nat
mul' Zero _ acc = acc
mul' _ Zero acc = acc
mul' (Succ a) b acc = mul' a b (plus acc b)

-- * вычитание (без использования предыдущих)
-- При этом, sub a b | (a - b), если a >= b
--                   | 0        иначе

sub :: Nat -> Nat -> Nat
sub a Zero = a
sub Zero _ = Zero
sub (Succ a) (Succ b) = sub a b 

-- * деление (через вычитание, остаток можно выкинуть)
div :: Nat -> Nat -> Nat
div a b = div' a b Zero

div' :: Nat -> Nat -> Nat -> Nat
div' _ Zero _ = Zero
div' a b acc | less a b = acc 
             | otherwise = div' (sub a b) b (plus (Succ Zero) acc)

less :: Nat -> Nat -> Bool
less Zero _ = True 
less _ Zero = False
less (Succ a) (Succ b) = less a b

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем целыми числами (в математике -- жирная Z)

-- Придумайте тип для целых чисел, обладающий
-- тем свойством, что каждое целое число имеет в нём
-- уникальное представление.

data Positive = One
              | PSucc Positive
instance Show Positive where
    show = show . pToInt 

pToInt :: Positive -> Int
pToInt One = 1
pToInt (PSucc a) = 1 + (pToInt a)

pFromInt :: Int -> Positive
pFromInt x | x <= 1 = One
           | otherwise = PSucc (pFromInt (x - 1))

-- pApplyBinary :: (Nat -> Nat -> a) -> Positive -> Positive -> a
-- pApplyBinary f p1 p2 = f (pToInt p1) (pToInt p2)

pless :: Positive -> Positive -> Bool
pless _ One = False
pless One _ = True 
pless (PSucc a) (PSucc b) = pless a b

psub :: Positive -> Positive -> Positive
psub One _ = One
psub (PSucc a) One = a
psub (PSucc a) (PSucc b) = psub a b

pplus :: Positive -> Positive -> Positive
pplus One b = PSucc b 
pplus (PSucc a) b = pplus a (PSucc b)

pmul :: Positive -> Positive -> Positive
pmul a b = pmul' a b One -- extra One

pmul' :: Positive -> Positive -> Positive -> Positive
pmul' One One acc = acc -- subtracting One
pmul' One b acc = pmul' b One acc
pmul' (PSucc a) b acc = pmul' a b (pplus acc b)

pdiv :: Positive -> Positive -> Positive
pdiv a b = pdiv' (psub a b) b One

pdiv' :: Positive -> Positive -> Positive -> Positive
pdiv' a One acc = pplus acc a
pdiv' a b acc | pless a b = acc
              | otherwise = pdiv' (psub a b) b (pplus acc One)

data Integer = IZero
             | Plus Positive
             | Minus Positive 
instance Show Integer where
    show = show . zToInt

zToInt :: Integer -> Int
zToInt IZero = 0
zToInt (Minus a) = - (zToInt (Plus a))
zToInt (Plus a) = pToInt a

zFromInt :: Int -> Integer
zFromInt 0 = IZero
zFromInt x | x > 0 = Plus (pFromInt x)
           | otherwise = Minus (pFromInt (-x))

-- Реализуйте:
-- * сложение
zplus :: Integer -> Integer -> Integer
zplus IZero a = a
zplus a IZero = a 
zplus (Plus a) (Plus b) = Plus (pplus a b)
zplus (Plus a) (Minus b) = zsub (Plus a) (Plus b)
zplus (Minus a) (Plus b) = zsub (Plus b) (Plus a)
zplus (Minus a) (Minus b) = znegate (zplus (Plus a) (Plus b)) 

znegate :: Integer -> Integer
znegate IZero = IZero
znegate (Plus a) = Minus a
znegate (Minus a) = Plus a

-- * умножение
zmul :: Integer -> Integer -> Integer
zmul IZero _ = IZero
zmul _ IZero = IZero
zmul (Minus a) b = znegate (zmul (Plus a) b)
zmul a (Minus b) = znegate (zmul a (Plus b))
zmul (Plus a) (Plus b) = Plus (pmul a b)

-- * вычитание
zsub :: Integer -> Integer -> Integer
zsub IZero a = znegate a
zsub a IZero = a
zsub (Plus One) (Plus One) = IZero
zsub (Plus One) (Plus (PSucc b)) = znegate (Plus b) 
zsub (Plus (PSucc a)) (Plus One) = Plus a
zsub (Plus (PSucc a)) (Plus (PSucc b)) = zsub (Plus a) (Plus b)
zsub (Plus a) (Minus b) = zplus (Plus a) (Plus b)
zsub (Minus a) (Plus b) = znegate (zplus (Plus a) (Plus b))
zsub (Minus a) (Minus b) = zsub (Plus b) (Plus a)

-- * деление
zdiv :: Integer -> Integer -> Integer
zdiv a IZero = IZero
zdiv IZero a = IZero
zdiv (Minus a) b = znegate (zdiv (Plus a) b)
zdiv a (Minus b) = znegate (zdiv a (Plus b))
zdiv (Plus a) (Plus b) = Plus (pdiv a b)

-------------------------------------------
-- Избавляемся от встроенных типов.
-- Продолжаем рациональными числами

-- Придумайте тип для рациональных чисел.
-- Уникальность представления каждого числа не обязательна.

data Rational = Rational Integer Positive
    deriving Show

peq :: Positive -> Positive -> Bool
peq One One = True
peq One b = False
peq a One = False
peq (PSucc a) (PSucc b) = peq a b

pgcd :: Positive -> Positive -> Positive
pgcd a b | peq a b = a
         | pless a b = pgcd a (psub b a)
         | otherwise = pgcd b (psub a b) 

rnorm :: Rational -> Rational
rnorm (Rational IZero _) = Rational IZero One
rnorm (Rational (Minus a) b) = Rational (Minus (pdiv a divisor)) (pdiv b divisor)
                             where divisor = pgcd a b
rnorm (Rational (Plus a) b) = Rational (Plus (pdiv a divisor)) (pdiv b divisor)
                            where divisor = pgcd a b

-- Реализуйте:
-- * сложение
rplus :: Rational -> Rational -> Rational
rplus (Rational i1 p1) (Rational i2 p2) = rnorm (Rational (zplus (zmul i1 (Plus p2)) (zmul i2 (Plus p1))) (pmul p1 p2))

-- * умножение
rmul :: Rational -> Rational -> Rational
rmul (Rational i1 p1) (Rational i2 p2) = rnorm (Rational (zmul i1 i2) (pmul p1 p2))

-- * вычитание
rsub :: Rational -> Rational -> Rational
rsub (Rational i1 p1) (Rational i2 p2) = rnorm (Rational (zsub (zmul i1 (Plus p2)) (zmul i2 (Plus p1))) (pmul p1 p2))

-- * деление
rdiv :: Rational -> Rational -> Rational
rdiv (Rational i1 p1) (Rational i2 p2) = case (zmul i2 (Plus p1)) of
                                         Plus a -> rnorm (Rational (zmul i1 (Plus p2)) a)
                                         Minus a -> rnorm (Rational (zmul i1 (Minus p2)) a)
                                         _ -> Rational IZero One 

-------------------------------------------
-- Конструируем типы.
-- Пары

-- Предположим, мы хотим уметь определять пары
-- элементов различных типов.
-- Мы могли бы делать это так:
data PairIntInt = PairII Int Int
data PairIntDouble = PairID Int Double
-- но это как-то грустно.

-- Магия Haskell позволяет нам сделать что-то типа
-- data Pair = \a b -> Pair#a#b a b
-- где #a и #b -- значения типовых переменных a и b,
-- или, даже лучше, просто
-- data Pair = \a b -> Pair a b
-- записывают это так:
data Pair a b = Pair a b
    deriving Show

-- Теперь
-- Pair Int Int
-- и
-- Pair Int Double
-- это разные типы, которые как бы генерируются компилятором.
--
-- Почти ничем не отличается от обычных функций (\x y -> y), функция Pair
-- имеет своим результатом тип с одним конструктором, который принимает два
-- аргумента соответствующих типов и возвращает упакованную из них пару.
-- Очень похоже на шаблонные параметры в C++.

-- Теперь мы можем писать функции, возвращающие несколько значений,
-- упакованных в пару.
example5 x = Pair x (Succ Zero)
-- Заметим, что это эквивалентно двум функциям:
example5'1 x = x
example5'2 x = Succ Zero

-- Следите за руками:
example6 x = let (Pair a b) = example5 x in a + (toInt b)
example6' x = let a = example5'1 x
                  b = example5'2 x in a + (toInt b)

-- Смотрите, я раньше сказал, что конструктор -- это почти функция.
-- Так вот он <<почти>> как раз потому, что его можно распаковывать
-- обратно.

-- Заметим также, что:
-- функция с типом
-- Pair a b -> c
-- имеет эквивалентного соседа с типом
-- a -> b -> c

example7 (Pair a b) = example7' a b
example7' a b = b

-- Это преобразование можно автоматизировать:
-- (напоминаю, что стрелка в типах правоассоциативна!)
curry :: (Pair a b -> c) -> a -> b -> c
curry f a b = f (Pair a b)

-- Реализуйте обратную:
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (Pair a b) = f a b 

-- Просто какие-то примеры.
example8 (Pair Zero (Succ _)) = Succ Zero
example8 (Pair Zero Zero) = Zero
example8 (Pair (Succ a) _) = a

example9 (Pair (Pair a b) c) = Pair a (Pair b c)

-- Реализуйте функцию pmap с типом
pmap :: (a -> a') -> (b -> b') -> Pair a b -> Pair a' b'
pmap f g (Pair a b) = Pair (f a) (g b)
-- делающую что-то разумное.

-------------------------------------------
-- Конструируем типы.
-- Списки, деревья

data List a = Cons a (List a) -- Элемент и хвост
            | Nil             -- Конец списка
    deriving Show

example10 = Cons (Succ Zero) $ Cons Zero $ Nil

length Nil = 0
length (Cons _ b) = 1 + length b

-- Реализуйте функцию map с типом
map :: (a -> b) -> List a -> List b
map f Nil = Nil
map f (Cons a list) = Cons (f a) (map f list) 
-- делающую что-то разумное и такую, что length l == length (map f l)

data Tree a = Node a (Tree a) (Tree a) -- Узел
            | Leaf                     -- Лист
    deriving Show

max a b = if a >= b then a else b

height Leaf = 0
height (Node _ a b) = 1 + max (height a) (height b)

-- Реализуйте функцию
tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf = Leaf
tmap f (Node a t1 t2) = Node (f a) (tmap f t1) (tmap f t2) 
-- делающую что-то разумное и такую, что height t == height (tmap f t)

-- Реализуйте функцию
list2tree :: List a -> Tree a
list2tree Nil = Leaf
list2tree (Cons a list) = Node a (list2tree list) Leaf
-- делающую что-то разумное и такую, что length l == height (list2tree l)

-------------------------------------------
-- Конструируем типы.
-- Логическое или

data Maybe a = Just a
             | Nothing
    deriving Show

-- Реализуйте функцию
find :: (a -> Bool) -> List a -> Maybe a
find p Nil = Nothing
find p (Cons a list) | p a = Just a
                     | otherwise = find p list 
-- которая ищет в списке t элемент, удовлетворяющий предикату p (если такой есть).

-- Реализуйте функцию
filter :: (a -> Bool) -> List a -> List a
filter p Nil = Nil
filter p (Cons a list) | p a = Cons a (filter p list)
                       | otherwise = filter p list 
-- которая генерирует список из элементов t, удовлетворяющих предикату f.

isJust Nothing  = False
isJust (Just _) = True

-- При помощи filter, isJust и map реализуйте разумную функцию с типом
maybefilter :: List (Maybe a) -> List a
maybefilter list = map getJust (filter isJust list)

getJust :: (Maybe a) -> a
getJust (Just a) = a

-- подсказка:
-- map (\(Just a) -> a)
-- Что в этой функции (и подсказке) плохо?

-- Реализуйте разумную функцию
gfilter :: (a -> Maybe b) -> List a -> List b
gfilter f Nil = Nil
gfilter f (Cons a list) = case (f a) of
                          Just b -> Cons b (gfilter f list)
                          _ -> gfilter f list

-- При помощи неё реализуйте maybefilter':
maybefilter' :: List (Maybe a) -> List a
maybefilter' l = gfilter (\a -> a) l
-- не обладающую предыдущим недостатком.

data Either a b = Left a
                | Right b
    deriving Show

data Empty --Пустое множество

-- Реализуйте
maybe2either :: Maybe a -> Either Empty a
maybe2either Nothing = (let f = f in f)
maybe2either (Just a) = Right a

-- Реализуйте
emap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b'
emap f g (Left a) = Left (f a)
emap f g (Right b) = Right (g b)
