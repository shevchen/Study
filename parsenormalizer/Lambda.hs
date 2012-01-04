module Lambda where

import Prelude hiding (iterate, (++), elem)

type Variable = String
type Varible = Variable

-- Лямбда-терм
data Term = Var Variable
          | Abs Variable Term
          | App Term Term

toString :: Term -> String
toString (Var a)   = a
toString (Abs a b) = "lambda " ++ a ++ " -> (" ++ toString b ++ ")"
toString (App a b) = "(" ++ toString a ++ ") (" ++ toString b ++ ")"

instance Show Term where
    show = show . toString

-- Тип [ a ] == Типу List a
-- значение [] == значению Nil
-- значение [ a ] == значению Cons a Nil
-- конструктор (:) == конструктору Cons

-- Конкатенация двух списков
(++) :: [a] -> [a] -> [a]
[]     ++ b = b
(a:as) ++ b = a:(as ++ b)

-- Свободные переменные терма
free (Var v)    = [v]
free (Abs v t)  = filter (/= v) . free $ t -- /= это <<не равно>>
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
iterate :: (a -> a) -> a -> [a]
iterate f x = (:) x $ iterate f (f x)

-- Генерирует список имён, производных от v, не входящих в fv
newname :: [String] -> String -> String
newname fv v = head . filter (\x -> not $ elem x fv) . iterate ('_':) $ v

-- Обычная бета-редукция, хендлящая переименования переменных
betaReduct :: Variable -> Term -> Term -> Term
betaReduct var what term = case term of 
    Var v    -> subst var what term
    App t t' -> App (betaReduct var what t) (betaReduct var what t')
    Abs v t  -> if v == var then term else Abs nn $ betaReduct var what (subst v (Var nn) t)
        where nn = newname ((free term) ++ (free what)) v

betaRecuct = betaReduct

-- Нормализация нормальным порядком терма term
normal'step :: Term -> (Term, Bool)
normal'step (Var v)             = (Var v, False)
normal'step (Abs v t)           = let (rest, resb) = normal'step t in (Abs v rest, resb)
normal'step (App (Abs v t1) t2) = (betaReduct v t2 t1, True)
normal'step (App t1 t2)         = let (rest1, resb1) = normal'step t1 in
    if resb1 then (App rest1 t2, True) else 
        let (rest2, resb2) = normal'step t2 in (App rest1 rest2, resb2)

normal' :: Term -> Term
normal' term = case (normal'step term) of
    (t, False) -> t
    (t, True)  -> normal' t

-- Нормализация аппликативным порядком терма term
applicative'step :: Term -> (Term, Bool)
applicative'step (Var v)     = (Var v, False)
applicative'step (Abs v t)   = let (rest, resb) = applicative'step t in (Abs v rest, resb)
applicative'step (App t1 t2) = let (rest2, resb2) = applicative'step t2 in
    if resb2 then (App t1 rest2, True) else
        let (rest1, resb1) = applicative'step t1 in
            if resb1 then (App rest1 rest2, True) else
                case rest1 of
                    Abs i j -> (betaReduct i rest2 j, True)
                    _       -> (App rest1 rest2, False) 

applicative' :: Term -> Term
applicative' term = case (applicative'step term) of
    (t, False) -> t
    (t, True)  -> applicative' t

-- Маркер конца ресурсов
data TooLoong = TooLoong deriving Show

-- (*) Нормализация нормальным порядком терма term за неболее чем n шагов.
-- Результат: Или числа итераций недостаточно, чтобы достичь нормальной
-- формы. Или (число нерастраченных итераций, терм в нормальной форме).
-- 
normal :: Int -> Term -> Either TooLoong (Int, Term)
normal n term = if n < 0 then (Left TooLoong) else
    case (normal'step term) of
        (t, False) -> Right (n, t)
        (t, True)  -> normal (n - 1) t

-- (*) Аналогичная нормализация аппликативным порядком.
applicative :: Int -> Term -> Either TooLoong (Int, Term)
applicative n term = if n < 0 then (Left TooLoong) else
    case (applicative'step term) of
        (t, False) -> Right (n, t)
        (t, True)  -> applicative (n - 1) t

-- (***) Придумайте и реализуйте обобщённую функцию, выражающую некоторое
-- семейство стратегий редуцирования. В том смысле, что нормальная, нормальная
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
