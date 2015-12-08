{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib = undefined

fibs1 :: [Integer]
fibs1 = undefined

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = undefined

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList = undefined

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap = undefined

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat = undefined

sIterate :: (a -> a) -> a -> Stream a
sIterate = undefined

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons _ _) _ = undefined

sTake :: Int -> Stream a -> [a]
sTake = undefined

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = undefined

ruler :: Stream Integer
ruler = undefined

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = undefined

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
