-- Зад.1.
sumProducts :: Num a => [[a]] -> a
sumProducts ll = sum (map product nonEmptyLists)
  where nonEmptyLists = filter (\lst -> not (null lst)) ll

-- Зад.2.
occurrences :: (Num a, Eq a) => [a] -> [a] -> [Int]
--occurrences lst1 lst2 = map (\x -> count x lst2) lst1
occurrences lst1 lst2 = [ count x lst2 | x<-lst1 ] -- същото като горния ред
  where count el lst = length (filter (\x -> x == el) lst)

-- Припомняне: транспониране на матрица
transpose :: [[a]] -> [[a]]
transpose m
  | null (head m) = [] -- в зависимост от обхождането "дъното" може да е и (null m)
  | otherwise     = (firstCol m) : transpose (removeFirstCol m)
  where firstCol m = map head m
        removeFirstCol m = map tail m

-- Зад.3.
mainDiag :: [[a]] -> [a]
mainDiag m
  | null m    = []
  | otherwise = (head (head m)) : mainDiag (tail (map tail m))

-- Зад.5.
sndDiag :: [[a]] -> [a]
sndDiag m = mainDiag (map reverse m)
-- sndDiag m = reverse (mainDiag (reverse m))

-- да проверим дали всички елементи в списък са равни помежду си
-- е като да проверим дали всички са равни на, например, първия елемент
allEqual :: (Num a, Eq a) => [a] -> Bool
allEqual lst = all (\x -> x == head lst) lst
-- естествено, ако говорим за числа, има и други начини
--allEqual lst = (minimum lst) == (maximum lst)

-- можем и с просто обхождане на списъка
allEqual2 [] = True
allEqual2 (_:[]) = True -- така pattern-match-ваме списък с точно 1 елемент
allEqual2 (x:y:rest)
  | x /= y    = False
  | otherwise = allEqual2 (y:rest)

-- Зад.4.
isSquare :: [[a]] -> Bool
isSquare m = allEqual (length m : map length m)

-- Зад.6.
matchLengths :: [[a]] -> Bool
matchLengths ll = allEqual (map length ll)

-- можем директно да сравняваме дали всички са "еквивалентни" на първия
-- но това ще преизчислява дължините при всяко сравнение!
matchLengths2 :: [[a]] -> Bool
matchLengths2 ll = all (\l -> length l == length (head ll)) ll

-- Зад.7.
-- Един подход при тези задачи е да "обхождаме" списъците подобно на merge
setUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
setUnion s1 [] = s1
setUnion [] s2 = s2
setUnion (x:xs) (y:ys)
  | x < y  = x : setUnion xs (y:ys)
  | x == y = x : setUnion xs ys
  | x > y  = y : setUnion (x:xs) ys

setIntersect s1 [] = []
setIntersect [] s2 = []
setIntersect (x:xs) (y:ys)
  | x < y  = setIntersect xs (y:ys)
  | x == y = x : setIntersect xs ys
  | x > y  = setIntersect (x:xs) ys

setDiff s1 [] = s1
setDiff [] s2 = []
setDiff (x:xs) (y:ys)
  | x < y  = x : setDiff xs (y:ys)
  | x == y = setDiff xs ys
  | x > y  = setDiff (x:xs) ys

setSymDiff s1 s2 = setUnion (setDiff s1 s2) (setDiff s2 s1)

-- а за някои функции можем просто да използваме filter или list comprehension
setIntersect2 s1 s2 = filter (\x -> x `elem` s2) s1
setDiff2 s1 s2 = filter (\x -> not (x `elem` s2)) s1

setIntersect3 s1 s2 = [ x | x<-s1, x `elem` s2]
setDiff3 s1 s2 = [ x | x<-s1, not x `elem` s2]

-- сортираща функция, която приема и функция за сравнение
quicksort :: (a -> a -> Bool) -> [a] -> [a]
quicksort _ [] = []
quicksort _ (x:[]) = [x]
--quicksort comp lst@(pivot:rest) =
--     quicksort comp (filter (\x -> p x pivot) rest)
--                 ++ (filter (\x -> not (p x pivot || p pivot x)) lst)
--  ++ quicksort comp (filter (\x -> p pivot x) rest)
-- по-красиво е с pattern matching
quicksort comp lst@(pivot:rest) =
       quicksort comp [ x | x<-rest, comp x pivot]
                   ++ [ x | x<-lst, not (comp x pivot), not (comp pivot x)]
    ++ quicksort comp [ x | x<-rest, comp pivot x]

-- обикновеното сортиране е просто с функцията (<)
quicksort' lst = quicksort (<) lst
-- пример за извикване със "специална" функция - да сравнява по последна цифра
--quicksort (\ x y -> x `mod` 10 < y `mod` 10)

-- Зад.8.
-- от списък с някакви елементи взимаме най-често срещания такъв.
-- ако има няколко най-често срещани, взимаме най-големия.
-- За тази задача ни е полезна функцията histogram:
removeAll :: (Eq a) => a -> [a] -> [a]
removeAll x l = filter (/=x) l

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (removeAll x xs)

histogram :: (Eq a) => [a] -> [(a,Int)]
histogram lst = map (\x -> (x,count x lst)) (removeDuplicates lst)
  where count x lst = length (filter (==x) lst)

-- за да изчислим най-често срещания елемент можем да вземем
-- хистограмата на списъка и да я сортираме по подходящ начин
mostFrequent :: (Eq a, Ord a) => [a] -> a
mostFrequent lst = fst (head sortedPairs)
  where pairs = histogram lst
        compPairs (x1,c1) (x2,c2) = c1 > c2 || (c1 == c2 && x1 > x2)
        sortedPairs = quicksort compPairs pairs

-- Вариант 1: супер бавен, тъй като при всяко сравнение преизчислява mostFrequent
-- но пък поне идеята на решението е ясна
specialSort :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort ll = quicksort (\x y -> mostFrequent x < mostFrequent y) ll

-- Вариант 2: веднъж изчисляваме за всеки x неговия mostFrequent,
-- после ги "окомплектоваме" в наредени двойки. След това сортираме
-- наредените двойки така, както искаме, и накрая от тях взимаме само
-- първите елементи (т.е. оригиналните елементи на списъка)
specialSort' :: (Eq a, Ord a) => [[a]] -> [[a]]
specialSort' ll = map fst sortedPairs
  where pairs = [ (x, mostFrequent x) | x<-ll ]
        compPairs (_,c1) (_,c2) = c1 < c2
        sortedPairs = quicksort compPairs pairs
