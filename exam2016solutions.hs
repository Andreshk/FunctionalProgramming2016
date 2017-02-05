import Data.List (maximumBy, delete)
import Data.Ord (comparing)

-- полезни функции и за двата варианта
transpose :: [[a]] -> [[a]]
transpose m = [ map (!!i) m | i<-[0..l]]
  where l = length (head m) - 1 -- pred . length . head $ m -- point-free, care-free

-- стандартни...
type Graph = [[Int]]
vertices :: Graph -> [Int]
vertices = map head
neighbours :: Int -> Graph -> [Int]
neighbours v g = tail $ head [ l | l<-g, (head l) == v ]

-- най-дълъг списък измежду списък от списъци
maxLength :: [[Int]] -> [Int]
maxLength = maximumBy $ comparing length

-- генериране на всички пътища в граф (!)
allPaths :: Graph -> [[Int]]
allPaths g = concat [ allPathsStartingWith [v] g | v<-(vertices g) ]

allPathsStartingWith :: [Int] -> Graph -> [[Int]]
allPathsStartingWith path g = path : concat [ allPathsStartingWith newPath g | newPath<-allExpansions]
  where allExpansions = [ path ++ [u] | u<-(vertices g), not $ u `elem` path, u `elem` (neighbours (last path) g) ]

------ Вар.А ------
-- Зад.1
findColumns :: Eq a => [[a]] -> Int
findColumns m = length [ col | col<-(transpose m), allInSomeRow col m ]
    -- дали за някой елемент на m (т.е. ред) е вярно, че всички елементи на lst (някоя колона) са в него
    where allInSomeRow :: Eq a => [a] -> [[a]] -> Bool
          allInSomeRow lst m = any (\row -> all (`elem` row) lst) m

-- Зад.2
-- сигнатурата не е необходима, но би трябвало да изглежда така:
combine :: (a -> b) -> (a -> c) -> (b -> c -> d) -> (a -> d)
combine f g h = \x -> h (f x) (g x)

-- дали за някои f,g,h от респективните им списъци и някое ff от uns е вярно, че "съвпадат"
-- където "съвпадението" се проверява от отделна помощна малка функция
check :: Int -> Int -> [(Int -> Int)] -> [(Int -> Int -> Int)] -> Bool
check a b uns bins = any id [ matches (combine f g h) ff [a..b] | f<-uns, g<-uns, h<-bins, ff<-uns ]
  where matches f1 f2 range = all (\x -> f1 x == f2 x) range

-- Зад.3
type Plant = (String,Int,Int)
plName :: Plant -> String
plName (n,_,_) = n
plMin :: Plant -> Int
plMin (_,m,_) = m
plMax :: Plant -> Int
plMax (_,_,m) = m

-- връща наредена двойка от търсения интервал и имената на растенията
-- идея: първо генерираме всички възможни интервали,
-- после за всеки съставяме списък от имената на растенията, които виреят в него,
-- и най-накрая на този списък взимаме максималния елемент по дължината на списъка с имена
garden :: [Plant] -> ((Int,Int),[String])
garden plants = maximumBy (comparing $ length . snd) [ (int, getNames int plants) | int<-allIntervals ]
  where allIntervals = [(minT,maxT) | minT<-(map plMin plants), maxT<-(map plMax plants), minT<maxT ]
        getNames int plants = [ plName p | p<-plants, growsIn int p ]
        growsIn (minT,maxT) (_,pMin,pMax) = pMin<=minT && maxT<=pMax

-- Зад.4
testGraph1 :: Graph
testGraph1 = [[1,2,4],[2,3],[3,2],[4]]

maxPath :: Graph -> Int -> [Int]
maxPath g v = maxPathHelper [v] (delete v (vertices g)) g

maxPathHelper :: [Int] -> [Int] -> Graph -> [Int]
maxPathHelper path unmarked g = if null possibles then path else maxLength possibles
  where possibles = [ maxPathHelper (expand path u) (delete u unmarked) g | u<-unmarked, canBeExpanded path u ]
        expand path u = path ++ [u]
        canBeExpanded path u = u `elem` (neighbours (last path) g)

-- Алтернативно решение - генерираме всички пътища (вж. най-горе) и взимаме най-дългия:
-- maxPath g v = maxLength [ p | p<-allPaths g, v == head p ]


------ Вар.2 ------
-- If you're having FP problems I feel bad for you, son
-- I got 2 exam variants, and that's the toughest one

-- Зад.1
hasColumn :: Eq a => [[a]] -> Bool
hasColumn m = any id [ allInAllRows col m | col<-(transpose m)]
  -- дали за всеки елемент на m (т.е. ред) е вярно, че всички елементи на lst (някоя колона) са в него
    where allInAllRows :: Eq a => [a] -> [[a]] -> Bool
          allInAllRows lst m = all (\row -> all (`elem` row) lst) m

-- Зад.2
-- сигнатурата не е необходима, но би трябвало да изглежда така:
--combine' :: (a -> b) -> (a -> c) -> (b -> c -> d) -> (a -> d)
combine' :: (a -> c -> d) -> (a -> b -> c) -> (a -> b) -> (a -> d)
combine' f g h = \x -> f x (g x (h x))

-- дали за някои f,g,h от респективните им списъци и някое ff от uns е вярно, че "съвпадат"
-- където "съвпадението" се проверява от отделна помощна малка функция
check' :: Int -> Int -> [(Int -> Int)] -> [(Int -> Int -> Int)] -> Bool
check' a b uns bins = any id [ matches (combine' f g h) ff [a..b] | f<-bins, g<-bins, h<-uns, ff<-uns ]
  where matches f1 f2 range = all (\x -> f1 x == f2 x) range

-- Зад.3
type Play = (String,Int,Int)
-- ще преизползваме plName от Plant типа: plName :: Play -> String; plName (n,_,_) = n
plStart :: Play -> Int
plStart (_,t,_) = t
plTime :: Play -> Int
plTime (_,_,t) = t

showtime :: [Play] -> ((Int,Int),[String])
showtime plays = maximumBy (comparing $ length . snd) [ (int, getNames int plays) | int<-allIntervals ]
  where allIntervals = [(i,i+1) | i<-[0..23]]
        getNames int plays = [ plName p | p<-plays, playsIn int p]
        playsIn (start,end) (_,pStart,pTime) = pStart<=start && end<=pStart+(pTime`div`60)

-- Зад.4
testGraph2 :: Graph
testGraph2 = [[1,2],[2,3],[3,1,4],[4,2]]

maxCycle :: Graph -> Int -> [Int]
maxCycle g v = maxLength [ p++[v] | p<-allPaths g, v == head p, v `elem` (neighbours (last p) g) ]

-- iei