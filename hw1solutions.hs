import Data.List (maximumBy, delete)
import Data.Ord (comparing)

-- Зад.1
hailstone :: Int -> [Int]
hailstone 1 = [1]
hailstone n = n : hailstone (if even n then div n 2 else 3*n + 1)

-- Зад.2
prime :: Int -> Bool
prime 1 = False
prime n = null [ d | d<-[2..sqn], (n `mod` d) == 0]
  where sqn = floor . sqrt . fromIntegral $ n

primes :: [Int]
primes = filter prime [1..]

isNotRepr :: Int -> Bool
isNotRepr n = null [ n | a<-[1..n], b<-[1..(n-a)], prime a, n == a + 2*b*b ]

result = length [ n | n<-[10..99], odd n, not $ prime n, isNotRepr n]

-- Зад.3
divisors :: Int -> [(Int,Int)]
divisors n = combinePairs $ allDivs n

-- функция, която връща гол списък от всички делители: allDivs 120 -> [2,2,2,3,5]
allDivs 1 = []
allDivs n = first : allDivs (n `div` first)
  where first = head $ filter (((==0) . (n `mod`))) primes -- point-free, care-free

combinePairs [] = []
combinePairs lst = (head lst, length firsts) : combinePairs rest
  where (firsts,rest) = span (== head lst) lst

-- Зад.4
intercalate' :: [a] -> [[a]] -> [a]
intercalate' _ [] = []
intercalate' _ (x:[]) = x
intercalate' m (x:xs) = x ++ m ++ intercalate' m xs

-- Зад.5
-- Можем да си позволим да съдържаме стойности и в листата
data Tree = Leaf Int | Node Int Tree Tree

getValue :: Tree -> Int
getValue (Leaf x) = x
getValue (Node x _ _) = x

-- pretty-print: или как да включим типа Tree в typeclass-а Show
instance Show Tree where
  --show (Leaf x) = "{" ++ show x ++ "}"
  --show (Node x lt rt) = "{" ++ show x ++ " " ++ show lt ++ " " ++ show rt ++"}"
  show = showHelper 0
    where showHelper pad (Leaf x) = replicate pad ' ' ++ show x ++ "\n"
          showHelper pad (Node x lt rt) = showHelper (pad+4) lt
                                       ++ replicate pad ' ' ++ show x ++ "\n"
                                       ++ showHelper (pad+4) rt

-- първо превръщаме списъка от числа в списък от листа, който списък
-- после обединяваме по двойки докато не остане с 1 елемент
fenwick :: [Int] -> Tree
fenwick lst = helper . map Leaf $ lst

helper :: [Tree] -> Tree
helper (t:[]) = t
helper lst = helper (makePairs lst)
  where makePairs [] = []
        makePairs (x:y:rest) = (Node (getValue x + getValue y) x y) : makePairs rest

-- Зад.6
-- Две малки помощни функции, които значително помагат за четимостта на кода по-долу
glue :: String -> String -> String
glue s1 s2 = s1 ++ tail s2

isGlueable :: String -> String -> Bool
isGlueable s1 s2 = last s1 == head s2

-- За удобство - взимане на най-дълга дума измежду списък
maxLength :: [String] -> String
maxLength = maximumBy $ comparing length

-- Най-дългата възможна дума все трябва да започва с някоя от думите в words (ако има такива)...
longestWord :: [String] -> String
longestWord [] = ""
longestWord words = maxLength [ longestStartingWith w (delete w words) | w<-words ]

-- На всяка стъпка гледаме каква е най-дългата досега залепена дума, както и множеството от
-- "останалите" думи. Тогава първо проверяваме възможно ли е текущата дума да бъде разширена:
-- ако няма накъде да я разширим дори с една думичка, връщаме самата нея, а ако е възможно - 
-- измежду всички възможности за разширение (изчислени рекурсивно надолу) избираме най-дългата.
-- Естествено, при рекурсивното извикване премахваме слепената думичка w от кандидатите (един вид,
-- маркираме я като използвана).
-- Тази е на практика задача за най-дълъг път в граф.
longestStartingWith :: String -> [String] -> String
longestStartingWith start words = if null possibles then start else maxLength possibles
  where possibles = [ longestStartingWith (glue start w) (delete w words) | w<-words, isGlueable start w ]
