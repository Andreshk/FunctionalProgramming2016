Целият текст във файл с разширение .lhs по подразбиране е коментар.
Редовете с истински код трябва да бъдат започнати с \"> " и отделени отделени
от коментарите с поне един празен ред.

Константите в Хаскел изглеждат така, както очакваме да изглежда някоя константа.
Можем изрично да посочим типа ѝ, ако не - компилаторът ще се досети кой е.

> mypi :: Double
> mypi = 3.141592

Сигнатура на функцията - ако я напишем, трябва да е пълна, т.е. или да пише
конкретните типове (Int,Float,Double,Bool,Char,String,...), или да включва
ограничения за тях в зависимост кои други функции използват тези аргументи.
Ако не я напишем, интерпретаторът ще я deduce-не вместо нас, със все ограничения.

Точна сигнатура (ще работи само с Double):
f :: Double -> Double -> Double
Обща, но грешна сигнатура (ще работи със всякакви типове - добре, но + и sqrt не работят със всякакви):
f :: a -> a -> a
Правилна сигнатура с ограничения за типа:
f :: (Floating a) => a -> a -> a


> f :: (Floating a) => a -> a -> a
> f x y = sqrt (x*x + y*y)
> abc :: Int
> abc = 3
> b :: Float
> b = 4.0

Функция, която приема някакво число и изписва какъв му е знака. Извиква оператора <,
който изисква типът да е "сравним", значи и нашата функция трябва да има същото ограничение.
Guard-овете трябва да се индентирани спрямо дефиницията на функцията (няма значение с колко)

> saySign :: (Num a, Ord a) => a -> String
> saySign x
>   | x < 0     = "Negative"
>   | x == 0    = "Zero"
>   | otherwise = "Positive"

Тривиално рекурсивно изчисление на n-тото число на Фибоначи.
Integral класът се включва в Ord, значи няма нужда да пишем (Ord a, Integral a)

> fib :: (Integral a) => a -> a
> fib n = if (n < 2)
>         then n
>         else (fib (n-1)) + (fib (n-2))

Изчисление с помощна функция, която дефинираме локално с where дефиниция (също индентирана)

> fib' :: (Integral a) => a -> a
> fib' n
>   | n < 0 = 0
>   | otherwise = fibHelper 0 1 startIdx
>   where fibHelper f1 f2 i
>           | i == n    = f1
>           | otherwise = fibHelper f2 (f1+f2) (i+1)
>         startIdx = 0

Функция, която брои колко корена има квадратно уравнение по дадени коефициенти

> countRoots :: (Ord a, Floating a) => a -> a -> a -> String
> countRoots a b c
>  | a == 0      = "Not a quadratic equation!"
>  | d < 0       = "zero roots"
>  | d == 0      = "one root"
>  | otherwise   = "two roots"
>  where d = b*b - 4*a*c

Отделните аргументи може да са различни типове с различни ограничения.

> power :: (Num a, Integral b) => a -> b -> a
> power base n
>   | n == 0    = 1
>   | even n    = square (power base (div n 2))
>   | otherwise = base * square (power base (div n 2))
>   where square x = x*x

Pattern matching - разписваме граничните случаи като отделни дефиниции.
Искаме те да бъдат преди "общия случай" на функцията по същата логика,
по която правим първо проверките в рекурсивната функция - нов е само синтаксиса.

> fib'' :: (Integral a) => a -> a
> fib'' 0 = 0
> fib'' 1 = 1
> fib'' n = fib'' (n-1) + fib'' (n-2)

Безполезна функция, която връща 0 ако някой от аргументите й е 0 и сбора на всички иначе.
Може в отделните случаи да ни интересуват само някои аргументи - на другите дори не даваме име.

> useless 0 _ _ _ = 0
> useless _ 0 _ _ = 0
> useless _ _ 0 _ = 0
> useless _ _ _ 0 = 0
> useless a b c d = a + b + c + d
