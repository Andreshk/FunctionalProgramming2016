import Data.List

type Point = (Double, Double)

points :: [Point]
points = [(2,8),(-2,4),(1,2),(-4,-1),(5,0)]

-- Зад.1 - Варианти А и Б
-- Първа стъпка - функция, която изчислява разстоянието между две точки (на квадрат)
dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = (x1-x2)^2 + (y1-y2)^2

-- Втора стъпка - за дадена точка да изчислим търсената сума от кв. на разст. до останалите
sumDists :: Point -> [Point] -> Double
sumDists p pts = sum $ map (dist p) pts

-- след като можем на всяка точка да съпоставяме тази сума, оттук нататък задачата
-- се свежда до търсенето на минимум/максимум по специален начин в списък - начини много
findMedoid :: [Point] -> Point
findMedoid pts = minimumBy (\p1 p2 -> compare (sumDists p1 pts) (sumDists p2 pts)) pts

-- (грозна) алтернатива - просто линейно обхождане
-- (на всяка стъпка трябва да помним и пълния списък с точки)
--findMedoid pts = helper (head pts) (tail pts) pts
--  where helper currMin [] _ = currMin
--        helper currMin (x:rest) allPts
--          | sumDists x allPts < sumDists currMin allPts = helper x rest allPts
--          | otherwise                                   = helper currMin rest allPts

-- аналогични решения и за другия вариант
findPoint :: [Point] -> Point
findPoint pts = maximumBy (\p1 p2 -> compare (sumDists p1 pts) (sumDists p2 pts)) pts

-- Зад.2 - Вариант А
-- за всеки k,n изчисляваме i-тата позиция в потока чрез предишните няколко
-- бавно, неефективно, малко неинтуитивно, но при внимателно индексиране - работещо
sumLast :: Integer -> Integer -> [Integer]
sumLast k n = map (\i -> fn k n i) [1..]
  where fn k _ 1 = k -- първото число е 2, независимо от колко "назад" търсим
        fn k n i = sum [ fn k n j | j<-[(max 1 i-n)..(i-1)] ] -- за да избегнем отрицателни индекси

-- идея - първо изчисляваме първите n елемента на потока,
-- след което за всяка следваща позиция пазим "прозореца"
-- от предишните n стойности. За този прозорец изчисляваме сумата,
-- добавяме в резултата и после преместваме прозореца.
sumLast2 :: Integer -> Integer -> [Integer]
sumLast2 k n = prefix ++ (helper prefix)
  where helper window = let currSum = sum window
                        in currSum : helper ((tail window) ++ [currSum])
        -- с малко съображение виждаме, че първите n числа са последователни
        -- степени на двойката, умножени по k.
        prefix = map (k*) $ 1 : map (2^) [0..n-2]

-- мисля, че вариант Б е очевидно аналогичен - заместете sum с product навсякъде

-- Зад.3. - Варианти А и Б
-- Припомняне - ориентиран граф съдържа ойлеров цикъл т.с.т.к. всички върхове
-- са с равни входящи и изходящи полустепени. Ойлеров път се съдържа ако всички
-- върхове без два са с равни полустепени, точно за един полустепента на входа
-- е с 1 повече от тази на изхода, и за точно един важи обратното (sink&source)
-- Всеки такъв цикъл или път (ако съществува) минава през всички върхове, тоест
-- търсим просто сумата на теглата във върховете.
-- Очевидно е НАЙ-удобно за всеки връх да намерим тези полустепени и да "търсим" в тях.
-- Ако намерим такъв цикъл/път, чак тогава връщаме сумата
type Graph = [(Char,Int,[Char])]
g :: Graph
g = [('a', 2, "bc"),
     ('b', 4, "ac"),
     ('c', 1, "ab")]

-- да живее list comprehension
outDeg :: Char -> Graph -> Int
outDeg u g = head [ length nbs | (v,_,nbs)<-g, u==v] -- "филтрираме" графа

inDeg :: Char -> Graph -> Int
inDeg u g = length [ (v,w,nbs) | (v,w,nbs)<-g, elem u nbs] -- всички върхове v, за които u е съсед

-- за всеки връх - наредени двойки от полустепените
insOuts :: Graph -> [(Int,Int)]
insOuts g = [ (inDeg u g, outDeg u g) | (u,_,_)<-g ]

sumWeights :: Graph -> Int
sumWeights g = sum [ w | (_,w,_)<-g ]

-- една лесна проверка
hasEulerCycle :: Graph -> Bool
hasEulerCycle g = all (\(i,o) -> i == o) degrees
  where degrees = insOuts g

-- 3 лесни проверки
hasEulerPath :: Graph -> Bool
hasEulerPath g = length [ (i,o) | (i,o)<-degrees, i /= o] == 2  -- точно два върха с различни in/out
              && length [ (i,o) | (i,o)<-degrees, i == o+1] == 1 -- точно един с in=out+1
              && length [ (i,o) | (i,o)<-degrees, i == o-1] == 1 -- точно един с in=out-1
  where degrees = insOuts g

eulerCycleCost :: Graph -> Int
eulerCycleCost g = if hasEulerCycle g then sumWeights g else 0

eulerCyclePath :: Graph -> Int
eulerCyclePath g = if hasEulerPath g then sumWeights g else 0

-- Зад.4. - Варианти А и Б
data Tree = Empty | Node Int Tree Tree deriving Show

makeLeaf :: Int -> Tree
makeLeaf x = Node x Empty Empty

testTree :: Tree
testTree = Node 3 (Node 5 (makeLeaf 2)
                          (Node 0 (makeLeaf 1)
                                  (makeLeaf 2)))
                  (Node 1 (makeLeaf 4)
                          Empty)

sumTree :: Tree -> Int
sumTree Empty = 0
sumTree (Node v l r) = v + sumTree l + sumTree r

countTree :: Tree -> Int
countTree Empty = 0
countTree (Node _ l r) = 1 + countTree l + countTree r

-- наивни, "бавни" решения
transformSum :: Tree -> Tree
transformSum Empty = Empty
transformSum t@(Node v l r) = (Node (sumTree t) (transformSum l) (transformSum r))

transformCount :: Tree -> Tree
transformCount Empty = Empty
transformCount t@(Node v l r) = (Node (countTree t) (transformCount l) (transformCount r))

-- за бонуса
getFakeRoot :: Tree -> Int
getFakeRoot Empty = 0
getFakeRoot (Node v _ _) = v

-- генерираме първо поддърветата и после за константно време
-- намираме стойността във съотв. възел
transformSum1 :: Tree -> Tree
transformSum1 Empty = Empty
transformSum1 (Node v l r) = (Node (v+lsum+rsum) newl newr)
  where newl = transformSum1 l
        newr = transformSum1 r
        lsum = getFakeRoot newl
        rsum = getFakeRoot newr

-- втора идея: помощна функция, която да връща полученото дърво
-- в наредена двойка със стойността си в корена (дори да звучи тъпо)
transformSum2 :: Tree -> Tree
transformSum2 t = fst $ helper t

helper :: Tree -> (Tree,Int)
helper Empty = (Empty, 0)
helper (Node v l r) = ((Node newv newl newr), newv)
  where (newl,lsum) = helper l
        (newr,rsum) = helper r
        newv = v + lsum + rsum

-- вариант Б също е очевидно аналогичен
