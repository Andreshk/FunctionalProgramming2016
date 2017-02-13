import Data.List (minimumBy, maximumBy, nub)
import Data.Ord (comparing)

-- и за двата варианта: в решенията се използва на няколко места функцията nub,
-- която премахва повторенията в даден списък. Ако не знаете, че тя е библиотечна,
-- винаги бихте могли да се я имплементирали сами по следния начин:
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (filter (/=x) xs)

-- и за двата варианта: стандартно шаблонно двоично дърво
data Tree a = Empty | Node a (Tree a) (Tree a)
-- не е необходимо, но помага: pretty-printing на дърво
instance Show a => Show (Tree a) where
  show = showHelper 0
    where showHelper pad Empty = replicate pad ' ' ++ "#\n"
          showHelper pad (Node x lt rt) = showHelper (pad+4) lt
                                       ++ replicate pad ' ' ++ show x ++ "\n"
                                       ++ showHelper (pad+4) rt

testTree :: Tree Int
testTree = Node 5
                (Node 3
                      (Node 1
                            Empty
                            (Node 2 Empty Empty))
                      (Node 4 Empty Empty))
                (Node 6 Empty Empty)

{----- Вар.А -----}
-- Зад.1
longestInterval :: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> (Int, Int)
longestInterval f g a b = maximumBy (comparing (\(a,b) -> b-a)) [ i | i<-allIntervals, everywhereMatch f g i]
  where allIntervals = [(from,to) | from<-[a..b], to<-[from..b]]
        everywhereMatch f g (from,to) = null [ x | x<-[from..to], f x /= g x ]

-- Бонус решение на Зад.1, което работи коректно и когато няма такива интервали
longestInterval' :: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> Maybe (Int, Int)
longestInterval' f g a b = if null intervals
                           then Nothing
                           else Just $ maximumBy (comparing (\(a,b) -> b-a)) intervals
  where allIntervals = [(from,to) | from<-[a..b], to<-[from..b]]
        everywhereMatch f g (from,to) = null [ x | x<-[from..to], f x /= g x ]
        intervals = [ i | i<-allIntervals, everywhereMatch f g i]

-- Зад.2
-- помощни функцийки
min3 a b c = min a (min b c)
max3 a b c = max a (max b c)

-- най-малкият интервал, съдържащ всички стойности, очевидно е между най-малката и най-голямата стойност в дървото
intervalTree :: Tree Int -> Tree (Int, Int)
intervalTree Empty = Empty
intervalTree tr@(Node _ left right) = Node (findMin tr, findMax tr)
                                           (intervalTree left)
                                           (intervalTree right)
  where findMin Empty = maxBound -- можем да използваме най-малката валидна стойност, без да попречи на коректността
        findMin (Node val left right) = min3 val (findMin left) (findMin right)
        findMax Empty = minBound
        findMax (Node val left right) = max3 val (findMax left) (findMax right)

-- Бонус: отново първо генерираме трансформираните поддървета, и от тях "изваждаме" необходимата информация за цялото дърво.
intervalTree' :: Tree Int -> Tree (Int, Int)
intervalTree' Empty = Empty
intervalTree' tr@(Node val left right) = Node (min3 val minLeft minRight, max3 val maxLeft maxRight)
                                              newLeftTree
                                              newRightTree
  where newLeftTree = intervalTree' left
        newRightTree = intervalTree' right
        (minLeft, maxLeft) = case newLeftTree of Empty -> (maxBound,minBound) -- забележете "обърнатите" стойности
                                                 (Node (a,b) _ _) -> (a,b)
        (minRight, maxRight) = case newRightTree of Empty -> (maxBound, minBound)
                                                    (Node (a,b) _ _) -> (a,b)

-- Зад.3
-- бързо за написване решение, но с малка уловка: Забележете, че b стига само до a и не го надхвърля;
-- ако беше a<-[1..], b<-[1..], тогава а никога няма да достигне 2 и няма да се генерират всички числа!
-- Накрая, функцията nub премахва повторенията:
sumOfSquares1 :: [Int]
sumOfSquares1 = nub [ a^2 + b^2 | a<-[1..], b<-[1..a] ]

-- а може и с просто филтриране (но работи доооста по-бавно):
sumOfSquares2 :: [Int]
sumOfSquares2 = [ n | n<-[1..], isRepr n ]
  where isRepr n = not $ null [ (a,b) | a<-[1..n], b<-[1..a], a^2+b^2==n ]

-- Зад.4
-- След като изчислим отделно средната дължина, можем първо да филтрираме тези клипове,
-- които са по-къси от средното, след което да вземем името на този с минимална разлика
-- между неговото време и средното. fromIntegral се използва навсякъде при работа с
-- цели и дробни числа, тъй като самото средно може да не е цяло число.
averageVideo :: [(String,Int)] -> String
averageVideo vids = fst $ minimumBy (comparing (\(_,v) -> (fromIntegral v) - avg))
                        $ filter ((<avg) . fromIntegral . snd) vids -- (\(_,v) -> fromIntegral v < avg)
  where avg = fromIntegral (sum $ map snd vids) / fromIntegral (length vids)

{----- Вар.Б -----}
-- Зад.1
equalityTest :: Eq a => (Int -> a) -> (Int -> a) -> Int -> Int -> Int
equalityTest f g a b = length [ i | i<-allIntervals, nowhereMatch f g i]
  where allIntervals = [(from,to) | from<-[a..b], to<-[from..b]]
        nowhereMatch f g (from,to) = null [ x | x<-[from..to], f x == g x ]

-- Зад.2
-- На практика задачата е идентична на тази от вариант А, макар и с променено условие. Така че:
pairTree :: Tree Int -> Tree (Int, Int)
pairTree = intervalTree

-- и бонуса, естествено:
pairTree' :: Tree Int -> Tree (Int, Int)
pairTree' = intervalTree'

-- Зад.3: вж. коментарите на зад.3 във вариант А
sumOfCubes1 :: [Int]
sumOfCubes1 = nub [ a^3 + b^3 | a<-[1..], b<-[1..a] ]

-- а може и с просто филтриране (но работи доооста по-бавно):
sumOfCubes2 :: [Int]
sumOfCubes2 = [ n | n<-[1..], isRepr n ]
  where isRepr n = not $ null [ (a,b) | a<-[1..n], b<-[1..a], a^3+b^3==n ]

-- Зад.4
-- Тук е полезно първо да генерираме списъка с всички различни модели на обувки,
-- след което от този списък да вземем името, за което броя различни модели е най-голям.
-- Това изчисление на брой различни модели е най-удобно да се изнесе също във външна функция.
bestRange :: [(String, Int)] -> String
bestRange shoes = maximumBy (comparing getSizes) allNames
  where allNames = nub $ map fst shoes -- всички имена
        getSizes n = length $ nub $ filter ((==n) . fst) shoes -- брой различни размери за дадено име n 