module OtherPrelude where
import Prelude

-- Склеить два списка за O(length a)
(++) :: [a] -> [a] -> [a]
[] ++ b     = b
(x:xs) ++ b = x:(xs ++ b)

-- Список без первого элемента
tail :: [a] -> [a]
tail []     = undefined
tail (x:xs) = xs

-- Список без последнего элемента
init :: [a] -> [a]
init []     = undefined
init (x:[]) = []
init (x:xs) = x:(init xs)

-- Первый элемент
head :: [a] -> a
head []     = undefined
head (x:xs) = x

-- Последний элемент
last :: [a] -> a
last []     = undefined
last (x:[]) = x
last (x:xs) = last xs

-- n первых элементов списка
take :: Integer -> [a] -> [a]
take _ []     = []
take n (x:xs) = if n <= 0 then [] else x:(take (n - 1) xs)

-- Список без n первых элементов
drop :: Integer -> [a] -> [a]
drop _ []     = []
drop n (x:xs) = if n <= 0 then x:xs else drop (n - 1) xs

-- Копировать из списка в результат до первого нарушения предиката
-- takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile pred []     = []
takeWhile pred (x:xs) = if pred x then x:(takeWhile pred xs) else []

-- Не копировать из списка в результат до первого нарушения предиката,
-- после чего скопировать все элементы, включая первый нарушивший
-- dropWhile (< 3) [1,2,3,4,1,2,3,4] == [3,4,1,2,3,4]
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile pred []     = []
dropWhile pred (x:xs) = if pred x then dropWhile pred xs else x:xs

-- Разбить список в пару (найбольший префикс удовлетворяющий p, всё остальное)
span :: (a -> Bool) -> [a] -> ([a], [a])
span pred []     = ([], [])
span pred (x:xs) = if pred x then (x:first, second) else ([x], xs)
                   where (first, second) = span pred xs

-- Разбить список по предикату на (takeWhile p xs, dropWhile p xs),
-- но эффективнее
break :: (a -> Bool) -> [a] -> ([a], [a])
break = span

-- n-ый элемент списка (считая с нуля)
(!!) :: [a] -> Integer -> a
[] !! n     = error "!!: empty list"
(x:[]) !! n = if n == 0 then x else undefined
(x:xs) !! n = if n < 0 then undefined else if n == 0 then x else xs !! (n - 1)

-- Список задом на перёд
reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = (reverse xs):[x]

-- map f l = из первой лабораторной
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x):(map f xs)

-- (*) Все подсписки данного списка
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) = (map ((:) x) sub) ++ sub
                      where sub = subsequences xs 

-- (*) Все перестановки элементов данного списка
permutations :: [a] -> [[a]]
permutations [] = []
permutations (x:xs) = ?

-- Повторяет элемент бесконечное число раз
repeat :: a -> [a]
repeat a = a:(repeat a)


-- Левая свёртка
-- порождает такое дерево вычислений:
--         f
--        / \
--       f   ...
--      / \
--     f   l!!2
--    / \
--   f   l!!1
--  / \
-- z  l!!0
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Тот же foldl, но в списке оказываются все промежуточные результаты
-- last (scanl f z xs) == foldl f z xs
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f z []     = [z]
scanl f z (x:xs) = z:(scanl f (f z x) xs)

-- Правая свёртка
-- порождает такое дерево вычислений:
--    f
--   /  \
-- l!!0  f
--     /  \
--   l!!1  f
--       /  \
--    l!!2  ...
--           \
--            z
--            
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

-- Аналогично
-- head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z []     = [z]
scanr f z (x:xs) = (scanr f (f x z) xs):z

finiteTimeTest = take 10 $ foldr (:) [] $ repeat 1

-- Склеивает список списков в список
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ (concat xs)

-- Эквивалент (concat . map), но эффективнее
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f []     = []
concatMap f (x:xs) = (f x) ++ (concatMap f xs)

-- Сплющить два списка в список пар длинны min (length a, length b)
zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y):(zip xs ys)

-- Аналогично, но плющить при помощи функции, а не конструктором (,)
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _          = []
zipWith _ _ []          = []
zipWith f (x:xs) (y:ys) = (f x y):(zipWith f xs ys)

-- Интересные классы типов
class Monoid a where
    mzero :: a
    mappend :: a -> a -> a

instance Monoid [a] where
    mzero = []
    mappend = (++)

instance Monoid Integer where
    mzero = 0
    mappend = (+)

data MulInteger = Mult Integer

instange Monoid MulInteger where
    mzero = 1
    (Mult a) `mappend` (Mult b) = Mult $ a * b

-- Фолдабл
class MFoldable t where
    mfold :: Monoid a => t a -> a

-- Альтернативный фолдабл
class Monoid a => AMFoldable t a where
    amfold :: t a -> a
-- Изучите раздницу между этими двумя определениями.

-- Смотрите какой чит. Можно построить дерево только из элементов моноида.
data MTree a = Monoid a => MLeaf | MNode a (MTree a) (MTree a)

-- Выпишите тип этого выражения. Фигурирует ли в нём Monoid? Почему?
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
mterm = ?

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

-- Группа
class Group a where
    gzero :: a
    ginv  :: a -> a
    gmult :: a -> a -> a

class Group Integer where
    gzero = 0
    ginv a = -a
    gmult = (+)

class Group MulInteger where
    ?

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = ?
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

matsum = ?

matscalarmul = ?

matmul = ?

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
class ? ScalarField ? where
    ?

class ? VectorField ? where
    ?
