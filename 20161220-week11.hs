-- Зад.1. zip-ване на списъци - два варианта
-- Съществува такава функция с име zipWith
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f lst1 [] = []
zipWith' f [] lst2 = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- не пишем сигнатура - при компилация тя бива deduce-ната
zipWith2 f lst1 lst2
  | null lst1 || null lst2 = []
  | otherwise              = (f x y) : zipWith2 f xs ys
  where x = head lst1
        xs = tail lst1
        y = head lst2
        ys = tail lst2

-- Зад.2. - и такава функция има (takeWhile)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x = x : takeWhile' p xs
  | otherwise    = []

-- Зад.3. - отново я има вградена
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = (\x y -> f y x)

-- за да тестваме долните функции
testList = [1,1,2,3,3,3,4,2,2,2,1,1]

-- Зад.4 - "компресиране" на списък
-- отново два варианта - с използване на вградените функции и със собствена countMyHead
compress :: Eq a => [a] -> [(a, Int)]
compress [] = []
compress lst = ((head lst), firsts) : compress rest
  where firsts = length (takeWhile (\x -> x == (head lst)) lst)
        rest = dropWhile (\x -> x == (head lst)) lst

compress2 :: Eq a => [a] -> [(a, Int)]
compress2 [] = []
compress2 lst = ((head lst), firsts) : compress2 rest
  where firsts = countMyHead lst
        rest = drop firsts lst -- след като знаем колко пъти се повтаря главата,
                               -- знаем колко точно елемента трябва да drop-нем

-- pattern matching за празен списък, списък с точно 1 елемент и списък с поне 2 елемента
countMyHead :: Eq a => [a] -> Int
countMyHead [] = 0
countMyHead (x:[]) = 1
countMyHead (x:y:xs)
  | x == y     = 1 + countMyHead (y:xs)
  | otherwise  = 1

-- Зад.5. - най-дълъг подсписък от еднакви стойности
maxRepeated :: Eq a => [a] -> Int
maxRepeated lst = maximum (map snd compressed)
  where compressed = compress lst

-- Зад.6.
-- итеративен вариант - изискваме Eq заради elem, който използва ==
makeSet :: Eq a => [a] -> [a]
makeSet lst = helper lst []
  where helper [] result = result
        helper (x:xs) result
          | x `elem` result = helper xs result
          | otherwise       = helper xs (x : result)

-- рекурсивен вариант - взимаме главата на списъка
-- и после премахваме всички нейни срещания с filter
makeSet' :: Eq a => [a] -> [a]
makeSet' [] = []
makeSet' (x:xs) = x : makeSet (removeHead xs)
  where removeHead lst = filter (\x -> x /= (head lst)) lst

-- Зад.7.
histogram :: Eq a => [a] -> [(a, Int)]
histogram lst = [ (el, count el lst) | el<-set ]
  where set = makeSet lst
        count el lst = length (filter (\y -> y == el) lst)

-- Зад.8. - вариант с list comprehension за съставяне на всички двойки точки
maxDistance :: [(Double,Double)] -> Double
maxDistance points = maximum [ dist p1 p2 | p1<-points, p2<-points ]
  where dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- Зад.9 - безкраен списък от функции, всяка представляваща композиции на f, започвайки от 1
-- вътрешно си дефинираме n-кратна композиция на функция.
compositions :: (a -> a) -> [(a -> a)]          -- списък от функции, no big deal
compositions f = [ composeN f i | i<-[1..] ]
  where composeN :: (a -> a) -> Int -> (a -> a) -- можем да имаме сигнатури и за локално деф. ф-ции
        composeN f 1 = f
        composeN f n = f . (composeN f (n-1))

-- за ентусиастите:
compositions' :: (a -> a) -> [(a -> a)]
compositions' f = f : map (f.) (compositions' f)
