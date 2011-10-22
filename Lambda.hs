module Lambda where

import Prelude hiding (iterate, (++), elem)

type Variable = String
type Varible = Variable

-- Лямбда-терм
data Term = Var Variable
          | Abs Variable Term
          | App Term Term
          deriving (Show)

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[] ++ b = b
(a:as) ++ b = a:(as ++ b)

-- Свободные переменные терма
free (Var v) = [v]
free (Abs v t) = filter (/= v) . free $ t -- /= это <<не равно>>
free (App t t') = (free t) ++ (free t')

-- Заменить все вхождения переменной var на what в терме term
subst var what term = case term of
    Var v    -> if v == var then what else term
    Abs v t  -> if v == var then term else Abs v (subst var what t)
    App t t' -> App (subst var what t) (subst var what t')

-- Содержит ли список элемент?
elem a [] = False
elem a (l:ls) = if a == l then True else elem a ls

-- Любопытная функция
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname fv v = head . filter (\x -> not . elem x $ fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = case term of 
    Var v    -> subst var what term
    App t t' -> App (betaReduct var what t) (betaReduct var what t')
    Abs v t  -> if v == var then term else Abs nn (betaReduct var what (subst v (Var nn) t))
        where nn = newname ((free t) ++ (free what)) v

betaRecuct = betaReduct

-- Нормализация нормальным порядком терма term
normal' :: Term -> Term
normal' term = if res then normal' newterm else term
    where (newterm, res) = normalstep term

normalstep :: Term -> (Term, Bool)
normalstep term = case term of
    Var v            -> (term, False)
    Abs v t          -> (Abs v newterm, res)
        where (newterm, res) = normalstep t
    App (Abs v t) t' -> (betaReduct v t' t, True)
    App t t'         -> if res then (App newterm t', True) else if res2 then (App t newterm2, True) else (term, False)
        where (newterm, res)   = normalstep t
              (newterm2, res2) = normalstep t'

-- Нормализация аппликативным порядком терма term
applicative' :: Term -> Term
applicative' term = if res then applicative' newterm else term
    where (newterm, res) = applicativestep term

applicativestep :: Term -> (Term, Bool)
applicativestep term = case term of
    Var v            -> (term, False)
    Abs v t          -> (Abs v newterm, res)
        where (newterm, res) = applicativestep t
    App (Abs v t) t' -> if res then (App (Abs v t) newterm, True) else if res2 then (App (Abs v newterm2) t', True) else (betaReduct v t' t, True)
        where (newterm, res)   = applicativestep t'
              (newterm2, res2) = applicativestep t
    App t t'         -> if res then (App t newterm, True) else if res2 then (App newterm2 t', True) else (term, False)
        where (newterm, res)   = applicativestep t'
              (newterm2, res2) = applicativestep t

-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
-- 
normal :: Int -> Term -> Either TooLoong (Int, Term)
normal n term = if n < 0 then Left TooLoong else if res then normal (n - 1) newterm else Right (n, term)
	where (newterm, res) = normalstep term

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative n term = if n < 0 then Left TooLoong else if res then applicative (n - 1) newterm else Right (n, term)
	where (newterm, res) = applicativestep term

-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что номальная, нормальная
-- головная, нормальная слабо-головная и аппликативная стратегии
-- при помощи этой функции будут выражаться некоторым элементарным образом.
-- Аргумент n можно отбросить, а можно оставить.
--
-- strategy = ?
--
-- normal = strategy ?
-- hnf = strategy ?
-- whnf = strategy ?
-- applicative = strategy ?
--
-- Какие ещё стратегии редуцирования вы знаете? Можно ли их выразить
-- при помощи этой стратегии? Если да, то как?
-- Если нет, то можно ли реализовать аналогичную функцию для _всех_
-- возможных стратегий редуцирования, а не только для такого семейства?
-- Если да, то как? Если нет, то почему?

--------------------------------------------------------

-- Область тестирования

loop' = Abs "x" $ App (Var "x") (Var "x")
loop = App loop' loop'

u = Abs "a" $ Abs "b" $ App (Var "a") $ App (Var "b") (Var "_b")
v = Abs "a" $ Abs "b" $ App (App (Var "a") (Var "b")) (Var "_b")
w = Abs "a" $ Abs "b" $ Abs "c" $ Abs "d" $ App (App (Var "a") (Var "b")) (App (Var "c") (Var "d"))

main = test 100
    [ ("no", normal)
    , ("ap", applicative) ]
    [ Var "a"
    , u
    , v
    , loop'
    , u `App` Var "a"
    , v `App` Var "a"
    , u `App` Var "b"
    , v `App` Var "b"
    , u `App` Var "_b"
    , v `App` Var "_b"
    , (u `App` Var "_b") `App` Var "_b"
    , (v `App` Var "_b") `App` Var "_b"
    , w
    , w `App` (Abs "a" (Var "a") `App` (Abs "b" $ Var "b"))
    , (w `App` Abs "a" (Var "b")) `App` loop
    , loop
    ]

-- Если вы не понимаете как это работает, то пока и не надо
pall n term  = mapM_ (\(desc, reduce) -> putStr (desc ++ ": ") >> print (reduce n term))

test :: Show a => Int -> [(String, Int -> Term -> a)] -> [Term] -> IO ()
test n funcs = mapM_ (\term -> print term >> pall n term funcs)
