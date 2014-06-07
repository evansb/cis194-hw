
-- CIS194 Homework 4
-- Evan Sebastian <evans@comp.nus.edu.sg>

fun1:: [Integer] -> Integer
fun1 = foldr (\x y -> if even x then (x - 2) * y else y) 1

fun2:: Integer -> Integer
fun2 = sum 
        . filter even
        . takeWhile (> 1)
        . iterate (\x -> if even x then x `div` 2 else x * 3 + 1)
        . (\x -> if even x then x * 2 else x)

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node height 
            (foldTree $ take half xs) 
        (xs !! half)
            (foldTree $ drop (half + 1) xs)
        where 
            len = length xs
            half = len `div` 2
            height = floor $ logBase 2 (fromIntegral len)

xor:: [Bool] -> Bool
xor = foldr (\x y -> (not x && y) || (x && not y)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\b g x -> g (f x b)) id xs base

sieveSundaram :: Int -> [Int]
sieveSundaram n = map (\x -> 2 * x + 1) $ removeij [1..n] 
    where
    possibleij:: Int -> (Int, [(Int, Int)])
    possibleij x = (x, [(i, j) | 
                   j <- [1..x `div` 3], 
                   i <- [1..j], 
                   i + j + 2 * i * j == x])
    removeij:: [Int] -> [Int]
    removeij = map fst 
                . filter (null . snd)
                . map possibleij
