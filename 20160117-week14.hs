-- Двоично дърво с цели числа във върховете
data IntTree = Empty | Node Int IntTree IntTree deriving Show

-- Дърво с произволен брой наследници (в списък) и цели числа във върховете
data VarTree = Empty' | Node' Int [VarTree]

vt :: VarTree
vt = Node' 5 [(Node' 2 []), Empty', (Node' 3 []), (Node' 4 []), (Node' 5 [])]

-- най-често при работата с такова дърво map-ваме нещо над всички
-- наследници на възела, и акумулираме получения списък със стойности
treeSum' :: VarTree -> Int
treeSum' Empty' = 0
treeSum' (Node' val children) = val + sum (map treeSum' children)

-- Втори вариант за представяне: 
data VarTree2 = Leaf Int | VNode Int [VarTree2]

vt2 :: VarTree2
vt2 = VNode 5 [Leaf 2, Leaf 3, Leaf 4, Leaf 5]
-- стандартни рекурсивни функции
-- много по-лесно се пишат и разбират, когато се
-- възползваме от pattern matching за конструкторите на дървото
-- в случая Empty символизира празното дърво, но на практика е конструктор (без аргументи)
treeSize :: IntTree -> Int
treeSize Empty = 0
treeSize (Node _ lt rt) = 1 + treeSize lt + treeSize rt

treeSum :: IntTree -> Int
treeSum Empty = 0
treeSum (Node val lt rt) = val + treeSum lt + treeSum rt

isEmpty :: IntTree -> Bool
isEmpty Empty = True
isEmpty _ = False

-- лош стил на програмиране - налага използването на други функции,
-- които не са способни да обработват всички "случаи" за дървета -> ЛОШО
--treeSum2 :: IntTree -> Int
--treeSum2 t = if isEmpty t then 0 else getValue t + ...

getValue :: IntTree -> Int
getValue Empty = 0 -- понеже функцията за сумиране разчита на тази!
getValue (Node val _ _) = val

-- за удобство
makeLeaf :: Int -> IntTree
makeLeaf x = Node x Empty Empty

testT1 :: IntTree
testT1 = Node 5 (Node 3 Empty
                        (makeLeaf 1))
                (makeLeaf 2)

-- задачката от контролното - O(n^2)
transformSum :: IntTree -> IntTree
transformSum Empty = Empty
transformSum t@(Node val lt rt) = Node (treeSum t)
                                       (transformSum lt)
                                       (transformSum rt)

-- O(n): предварително "генерираме" лявото и дясното поддърво, и от тях
-- "извличаме" стойността в корените им - така не преизчисляваме нищо
-- по повече от веднъж и функцията е очевидно линейна
transformSum' :: IntTree -> IntTree
transformSum' Empty = Empty
transformSum' (Node val lt rt) = Node (val + leftsum + rightsum)
                                      lefttree
                                      righttree
  where lefttree = transformSum' lt
        righttree = transformSum' rt
        leftsum = getValue lefttree
        rightsum = getValue righttree

-- Зад.1.
maxSumPath :: IntTree -> Int
maxSumPath Empty = 0
maxSumPath (Node val lt rt) = val + max (maxSumPath lt) (maxSumPath rt)

-- Зад.2.
prune :: IntTree -> IntTree
prune Empty = Empty
prune (Node _ Empty Empty) = Empty
prune (Node val lt rt)     = (Node val (prune lt) (prune rt))

-- Зад.3.
bloom :: IntTree -> IntTree
bloom Empty = Empty
bloom (Node val Empty Empty) = (Node val (makeLeaf val) (makeLeaf val))
bloom (Node val lt rt)       = (Node val (bloom lt) (bloom rt))

-- Дърво, което съдържа произволни стойности по върховете - 
-- приема съдържания тип като параметър подобно на C++.
-- Оттам нататък винаги дървото върви с типа си - не пишем BST,
-- както на пишем std::vector, ами "BST a", "BST Int" като
-- "std::vector<T>" и "std::vector<int>".
data BST a = BSTEmpty | BSTNode a (BST a) (BST a) deriving Show

-- Зад.4.
bstsize :: BST a -> Integer
bstsize BSTEmpty = 0
bstsize (BSTNode _ lt rt) = 1 + bstsize lt + bstsize rt

bstinsert :: (Eq a, Ord a) => a -> BST a -> BST a
bstinsert x BSTEmpty = BSTNode x BSTEmpty BSTEmpty
bstinsert x t@(BSTNode val lt rt)
  | x == val  = t
  | x < val   = (BSTNode val (bstinsert x lt) rt)
  | otherwise = (BSTNode val lt (bstinsert x rt))

bstsearch :: (Eq a, Ord a) => a -> BST a -> Bool
bstsearch _ BSTEmpty = False
bstsearch x (BSTNode val lt rt)
  | x == val  = True
  | x < val   = bstsearch x lt
  | otherwise = bstsearch x rt

bstFromList :: (Eq a, Ord a) => [a] -> BST a
bstFromList = foldr bstinsert BSTEmpty
-- bstFromList lst = foldr bstinsert BSTEmpty lst
-- bstFromList lst = foldl (flip bstinsert) BSTEmpty lst

testBST :: BST Int
testBST = bstFromList [5,3,6,2,4,8,7,1]

-- забележете - тази функция няма ограничения за съдържания тип!
values :: BST a -> [a]
values BSTEmpty = []
values (BSTNode val lt rt) = values lt ++ [val] ++ values rt

bstSort :: (Eq a, Ord a) => [a] -> [a]
bstSort = values . bstFromList
-- bstSort lst = values $ bstFromList lst

-- Зад.5.
-- нашият map отново ще представлява дърво, съдържащо наредени двойки
data Map k v = MEmpty | MNode (k,v) (Map k v) (Map k v)

mapinsert :: (Eq k, Ord k) => k -> v -> Map k v -> Map k v
mapinsert key val MEmpty = (MNode (key,val) MEmpty MEmpty)
mapinsert key val (MNode p@(k1,_) lt rt)
  | key == k1 = (MNode (k1,val) lt rt) -- заместваме старата с новата стойност
  | key < k1  = (MNode p (mapinsert key val lt) rt)
  | otherwise = (MNode p lt (mapinsert key val rt))

mapsearch :: (Eq k, Ord k) => k -> Map k v -> Maybe v
mapsearch _ MEmpty = Nothing
mapsearch key (MNode p@(k1,val) lt rt)
  | key == k1 = Just val
  | key < k1  = mapsearch key lt
  | otherwise = mapsearch key rt

mapFromList :: (Eq k, Ord k) => [(k,v)] -> Map k v
mapFromList = foldr (uncurry mapinsert) MEmpty           -- lol
-- mapFromList = foldl (flip $ uncurry mapinsert) MEmpty -- loool
-- mapFromList lst = foldr (\(key,val) m -> mapinsert key val m) MEmpty lst 

mapvalues :: Map k v -> [(k,v)]
mapvalues MEmpty = []
mapvalues (MNode p lt rt) = mapvalues lt ++ [p] ++ mapvalues rt
