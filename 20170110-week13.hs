import Data.List -- за функциите за специална подредба minimumBy, maximumBy

-- Зад.1.
-- псевдоним на тип - можем да използваме Point навсякъде
-- вместо (Double, Double), както и обратното
type Point = (Double, Double)

points :: [Point]
points = [(-1.1, 1), (1.8, 2), (3, 1), (-1, -2)]

dist :: Point -> Point -> Double
dist (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

maxDistance :: [Point] -> Double
maxDistance pts = maximum [ dist p1 p2 | p1<-pts, p2<-pts ]

-- бонус: да приемаме и функцията за разстояние като параметър
maxDistanceBy :: (Point -> Point -> Double) -> [Point] -> Double
maxDistanceBy f pts = maximum [ f p1 p2 | p1<-pts, p2<-pts]
-- тогава maxDistance = maxDistanceBy dist

-- ако искаме да върнем не само най-голямото разстояние,
-- ами и точките, между които е то: изграждаме списък (d,p1,p2)
-- и му взимаме елемента с най-голяма стойност на първата позиция
maxDistance1 :: [Point] -> (Double, Point, Point)
maxDistance1 pts = maximumBy (\ p@(d1,_,_) q@(d2,_,_) -> compare d1 d2) [ (dist p1 p2, p1, p2) | p1<-pts, p2<-pts ]
--                                                       ^^^^^^^^^^^^^

-- Зад.2.
type Item = (String, Integer)
items :: [Item]
items = [("Milk",3), ("Bread",1), ("Yoghurt",-3),
         ("Butter",5), ("Cheese",-1), ("Pasta",2)]

soonestExpiring :: [Item] -> String
soonestExpiring its = fst $ minimumBy
                          (\ i1 i2 -> compare (snd i1) (snd i2))
                          [ i | i<-its, snd i >= 0] -- можем да заменим този ред с някое от по-долните:
--                        filter (\i -> snd i >= 0) its
--                        filter ((>=0) . snd) its
--                        filter (\(_,d) -> d >= 0) its

numberExpiring :: [Item] -> Int
numberExpiring its = length [ i | i<-its, (snd i) < 0]

longestExpired :: [Item] -> String
longestExpired its = fst $ minimumBy
                         (\ i1 i2 -> compare (snd i1) (snd i2))
                         [ i | i<-its, snd i < 0 ]
-- алтернатива:
longestExpired1 :: [Item] -> String
longestExpired1 = fst . minimumBy (\ i1 i2 -> compare (snd i1) (snd i2))

expiringItems its = (soonestExpiring its, numberExpiring its, longestExpired its)

---------------------------------------------------------------------------------------------

-- създавайки типа NewPoint, трябва да указваме как конструираме обекти от този тип
-- в случая "кръщаваме" конструктора с думичката Pair (можеше и да е друга, дори и NewPoint)
data NewPoint = Pair Double Double

-- в такъв случай единственият начин да конструираме такива обекти
-- е като извикаме този конструктор
p1 :: NewPoint
p1 = Pair 3 5

-- естествено, можем да извикваме този конструктор отвсякъде
makePoint :: (Double, Double) -> NewPoint
makePoint (x,y) = Pair x y
--makePoint = uncurry Pair -- за ентусиастите

-- единственият начин, по който можем да достъпваме данните на тези обекти,
-- е като pattern match-ваме начина, по който са конструирани
-- например засега имаме само конструктора Pair, и по него "декомпозираме" обекта
getX :: NewPoint -> Double
getX (Pair x _) = x

getY :: NewPoint -> Double
getY (Pair _ y) = y

-- pattern matching в други функции
dist' :: NewPoint -> NewPoint -> Double
dist' (Pair x1 y1) (Pair x2 y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

-- може и само с get-ъри - както е по-удобно
dist1 :: NewPoint -> NewPoint -> Double
dist1 p1 p2 = sqrt $ ((getX p1) - (getX p2))^2 + ((getY p1) - (getY p2))^2

-- сега можем да пренапишем и maxDistance за новия тип данни:
-- първо конструираме списъка от точки по наредените двойки
points' :: [NewPoint]
points' = [ (Pair x y) | (x,y)<-points]

-- на практика същия код, но работим с друг тип и друга функция за разстояние
maxDistance' :: [NewPoint] -> Double
maxDistance' pts = maximum [ dist' p1 p2 | p1<-pts, p2<-pts ]
