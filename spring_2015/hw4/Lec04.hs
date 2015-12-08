{-# LANGUAGE MultiParamTypeClasses #-}
module Lec04 where

import Data.Functor
import Data.Monoid
{-
class Eq a where
    (==) :: a -> a -> Bool
    x == y = not (x /= y)
    (/=) :: a -> a -> Bool
    x /= y = not (x == y)
-}

data Foo = F Int
         | G Bool
           deriving (Show)

instance Eq Foo where
    F x == F y = x == y
    G x == G y = x == y
    _ == _ = False

f :: String -> String
f = show . (read :: String -> Int)

class Castable a b where
    cast :: a -> b

instance Castable Int Integer where
    cast = fromIntegral

maybeConcat :: Maybe String -> Maybe String -> Maybe String
maybeConcat (Just s1) (Just s2) = Just $ s1 ++ s2
maybeConcat Nothing (Just s)    = Just s
maybeConcat (Just s) Nothing    = Just s
maybeConcat Nothing Nothing     = Nothing

data X a = X a
           deriving (Show)

instance Functor X where
    fmap f (X a) = X $ f a

-- Style Examples -------------------------------------

isSorted :: Ord a => [a] -> Bool
isSorted []  = True
isSorted [x] = True
isSorted (x1 : x2 : xs)
    | x1 <= x2  = isSorted (x2 : xs)
    | otherwise = False

-- The original function had:
-- * Redundant base cases
-- * Unnecessary guards

isSorted' :: Ord a => [a] -> Bool
isSorted' (x1 : x2 : xs) = x1 <= x2 && isSorted' (x2 : xs)
isSorted' _ = True


countMistakes :: Eq a => [a] -> [[a]] -> [Int]
countMistakes ans []     = []
countMistakes ans (r:rs) =
    countMs ans r : countMistakes ans rs
    where countMs [] _  = 0
          countMs _  [] = 0
          countMs (x:xs) (y:ys)
              | x == y    = countMs xs ys
              | otherwise = 1 + countMs xs ys

-- The original version had:
-- * Explicit recursion patterns that can be replaced with `map` and `zipWith`

countMistakes' :: Eq a => [a] -> [[a]] -> [Int]
countMistakes' ans = map countMs
    where countMs = sum . zipWith (fromEnum .: (/=)) ans
