module OtherPrelude where
import Prelude( Show(..), Bool(..), Integer(..), Rational(..)
               , (+), (-), (*), (/)
               , (<), (==), (>), (<=), (>=)
               , not, (&&)
               , undefined, error, ($), (.) )

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
span pred (x:xs) = if pred x then (x:first, second) else ([], x:xs)
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
reverse l = reverse' l []

reverse' :: [a] -> [a] -> [a]
reverse' [] l     = l
reverse' (x:xs) l = reverse' xs (x:l)

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
permutations [] = [[]]
permutations (x:xs) = permutations' x [] xs

permutations' :: a -> [a] -> [a] -> [[a]]
permutations' x xs []     = map ((:) x) (permutations xs)
permutations' x xs (y:ys) = map ((:) x) (permutations (xs ++ (y:ys))) ++ (permutations' y (x:xs) ys)

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
--  head (scanr f z xs) == foldr f z xs.
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f z [] = [z]
scanr f z (x:xs) = (f x y):(y:ys)
                   where y:ys = scanr f z xs

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
data MulRational = RMult Rational

-- Реализуйте инстансы Monoid для Rational и MulRational
instance Monoid Rational where
    mzero = 0
    mappend = (+)

instance Monoid MulRational where
    mzero = RMult 1
    mappend (RMult a) (RMult b) = RMult (a * b)

instance Monoid MulInteger where
    mzero = Mult 1
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
mtfold :: Monoid a => MTree a -> a
mtfold MLeaf = mzero -- А то, что a - моноид нам будет даровано самой природой
mtfold (MNode a l r) = a `mappend` (mtfold l) `mappend` (mtfold r)

-- Напишите терм с типом
-- (...) => MTree a -> x
-- где x -- тип на ваш вкус, (...) - какие-то констреинты (возможно пустые),
-- при этом этот терм внутри должен использовать то, что a -- моноид, но в
-- констреинтах Monoid a быть не должно.
-- Для широты фантазии в терме можно использовать классы типов, определённые в любом
-- месте этого файла.
mterm :: MTree a -> MTree a
mterm MLeaf = MLeaf
mterm (MNode x t1 t2) = MNode x t1 t2

-- (**) Разберитесь чем отличаются эти определения.
-- "Скомпилируйте" их в наш гипотетический язык программирования с
-- типом Dict.
instance MFoldable MTree where
    mfold = mtfold

instance Monoid a => AMFoldable MTree a where
    amfold = mtfold

--------- Тут переделаем немного
-- Группа
--class Group a where
--    gzero :: a
--    ginv  :: a -> a
--    gmult :: a -> a -> a
--
--class Group Integer where
--    gzero = 0
--    ginv a = -a
--    gmult = (+)
--
--class Group MulInteger where
--    ? это я погорячился, да

-- Хаскель слабоват для нормального определения всех этих штук.
-- Кольцо вообще непонятно как определить, потому что группы и полугруппы
-- должны быть по паре (тип, операция).
class Monoid a => Group a where
    ginv :: a -> a

-- Определите
--instance Group для Integer, Rational, MulRational
instance Group Integer where
    ginv = (-) 0

instance Group Rational where
    ginv = (-) 0

instance Group MulRational where
    ginv (RMult a) = RMult (1 / a)

-- Группу и Абелеву группу в Хаскеле тоже не различить :(
class Group a => Ring a where
    -- mappend из моноида это сложение
    rmul :: a -> a -> a -- а это умножение

-- Определите
--instance Ring для Integer, Rational
instance Ring Integer where
    rmul = (*)

instance Ring Rational where
    rmul = (*)

-- На самом деле коммутативное кольцо, но что поделать
class Ring a => Field a where
    rinv :: a -> a

-- Определите
--instance Field для Rational
instance Field Rational where
    rinv = (/) 1

-- Реализуйте тип для матриц (через списки) и операции над ними
data Matrix a = Ring a => Matrix [[a]]
-- Чем должно быть a? Моноидом? Группой? Ещё чем-нибудь?

getTable :: Matrix a -> [[a]]
getTable (Matrix x) = x

instance Show a => Show (Matrix a) where
    show = show . getTable

matsum :: Matrix a -> Matrix a -> Matrix a
matsum (Matrix []) (Matrix [])         = Matrix []
matsum (Matrix []) _                   = undefined
matsum _ (Matrix [])                   = undefined
matsum (Matrix (x:xs)) (Matrix (y:ys)) = Matrix ((scalarsum x y):(getTable (matsum (Matrix xs) (Matrix ys))))

scalarsum :: Monoid a => [a] -> [a] -> [a]
scalarsum [] []         = []
scalarsum [] _          = undefined
scalarsum _ []          = undefined
scalarsum (x:xs) (y:ys) = (mappend x y):(scalarsum xs ys)

matscalarmul :: Ring a => a -> Matrix a -> Matrix a
matscalarmul _ (Matrix [])     = Matrix []
matscalarmul x (Matrix (y:ys)) = Matrix ((map (rmul x) y):(getTable (matscalarmul x (Matrix ys))))

matmul :: Matrix a -> Matrix a -> Matrix a
matmul (Matrix m1) (Matrix m2) = if has then Matrix (matrixPrepend (map (multiplicate fc) m1) (getTable (matmul (Matrix m1) (Matrix other)))) else Matrix []
                                    where (fc, other, has) = firstColumn m2

multiplicate :: Ring a => [a] -> [a] -> a
multiplicate [] [] = mzero
multiplicate [] _ = undefined
multiplicate _ [] = undefined
multiplicate (x:xs) (y:ys) = mappend (rmul x y) (multiplicate xs ys)

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False

firstColumn :: [[a]] -> ([a], [[a]], Bool)
firstColumn []          = ([], [], True) 
firstColumn ([]:xs)     = if has && not (isEmpty fc) then undefined else (fc, other, False)
                          where (fc, other, has) = firstColumn xs
firstColumn ((y:ys):xs) = if not has then undefined else (y:fc, ys:other, True)
                          where (fc, other, has) = firstColumn xs

matrixPrepend :: [a] -> [[a]] -> [[a]]
matrixPrepend [] []         = []
matrixPrepend [] _          = undefined
matrixPrepend (x:xs) []     = [x]:(matrixPrepend xs [])
matrixPrepend (x:xs) (y:ys) = (x:y):(matrixPrepend xs ys)

-- (**) Реализуйте классы типов для векторных и скалярных полей.
-- Перепишите в этих терминах что-нибудь из написанного выше.
-- Реализуйте оператор дифференцирования, взятия градиента.
-- class ? ScalarField ? where
--     ?

-- class ? VectorField ? where
--     ?
