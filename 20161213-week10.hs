-- Зад.1. - можем да pattern match-ваме по компонентите на наредените двойки
modulus :: Floating a => (a, a) -> a
modulus (x,y) = sqrt (x^2 + y^2)
-- без pattern matching:
-- modulus p = sqrt ((fst p)^2 + (snd p)^2)

-- можем да pattern match-ваме и наредени тройки, n-торки и т.н.
modulus3D :: Floating a => (a, a, a) -> a
modulus3D (x,y,z) = sqrt(x^2 + y^2 + z^2)

-- Зад.2.
complAdd :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
complAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

complSub :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
complSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

complMul :: Num a => (a, a) -> (a, a) -> (a, a)
complMul (x1,y1) (x2,y2) = (x1*x2 - y1*y2, x1*y2 + x2*y1)

-- Зад.3. - три решения с различните синтактични конструкции
-- pattenr matching
ackermann :: Integral a => a -> a -> a
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

-- при case можем да "разпознаваме" само един "обект",
-- така че обединяваме двата ни аргумента в наредена двойка
ackermann' :: Integral a => a -> a -> a
ackermann' m n = case (m,n) of (0,n) -> n + 1
                               (m,0) -> ackermann' (m-1) 1
                               (m,n) -> ackermann' (m-1) (ackermann' m (n-1))

-- guard-ове
ackermann2 :: Integral a => a -> a -> a
ackermann2 m n
  | m == 0     = n + 1
  | n == 0     = ackermann2 (m-1) 1
  | otherwise  = ackermann2 (m-1) (ackermann2 m (n-1))

-- класически вложени if-ове - изглеждат грозно на всички езици
ackermann3 :: Integral a => a -> a -> a
ackermann3 m n = if m == 0
                 then n+1
                 else (if n == 0
                       then ackermann3 (m-1) 1
                       else ackermann3 (m-1) (ackermann3 m (n-1)))

-- Зад.4.
distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

-- Зад.5.
replicate' :: Integral a => a -> b -> [b]
replicate' 0 _ = []
replicate' n el = el : (replicate' (n-1) el)

-- Зад.6. - можем да pattern match-ваме и списъци !
take' :: Integral a => a -> [b] -> [b]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (n-1) xs)
--      ^^^^^^

-- Зад.7. - list comprehension и range
prime :: Integral a => a -> Bool
prime 1 = False
prime n = null [ d | d<-[2..(n-1)], mod n d == 0 ]

-- Зад.8.
primes :: Integral a => [a]
primes = [ x | x<-[2..], prime x ]

-- Бонуси за list comprehension - декартово произведение на два списъка
descartes l1 l2 = [ (x,y) | x<-l1, y<-l2 ]

-- генериране на всички триъгълници с дължини на страните <=10
-- за повторение подреждаме страните само в нарастващ ред, тоест a<=b<=c
-- на практика е декартовото произведение на ТРИ списъка, макар и филтрирано
triangles = [ (a,b,c) | a<-[1..10], b<-[1..10], c<-[1..10],
                        a+b>c, a+c>b, b+c>a, a<=b, b<=c ]

-- втори вариант
triangles2 = [ (a,b,c) | c<-[1..10], b<-[1..c], a<-[1..b],
                        a+b>c, a+c>b, b+c>a ]

-- взимане на n-тия елемент на списък - има го и като вграден оператор !!
nth :: Integral a => a -> [b] -> b
nth 0 lst = head lst
nth n lst = nth (n-1) (tail lst) 

-- Зад.9.
-- забележка - !! изисква вторият аргумент (индексът) да е точно тип Int
nthPrime :: Integral a => Int -> a
nthPrime n = primes !! n

-- Зад.10. - take и drop аналогично изискват Int, иначе нямаме ограничения за типовете
removeNth :: Int -> [a] -> [a]
removeNth _ [] = [] -- бяхме забравили дъното на рекурсията !
removeNth n lst = (take (n-1) lst) ++ removeNth n (drop n lst)

-- Зад.11. Сито на Ератостен - просто красота
primes2 :: [Int]
primes2 = sieve [2..]
  where sieve (x:xs) = x : sieve (removeNth x xs)
        -- не се притесняваме за празни списъци, тъй като работим с безкрайни такива
