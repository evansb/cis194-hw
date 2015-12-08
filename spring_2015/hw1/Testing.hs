{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
module Testing where

import Data.Maybe
import Control.Arrow

data Test    = forall a. Show a => Test String (a -> Bool) [a]
data Failure = forall a. Show a => Fail String [a]

instance Show Failure where
    show (Fail s as) = "Failed Test \"" ++ s
                       ++ "\" on inputs " ++ show as

runTest :: Test -> Maybe Failure
runTest (Test s f as) = case filter (not . f) as of
                          [] -> Nothing
                          fs -> Just $ Fail s fs

runTests :: [Test] -> [Failure]
runTests = catMaybes . map runTest

-- Helpers

testF1 :: (Show a, Show b, Eq b) => String -> (a -> b) -> [(a, b)] -> Test
testF1 s f l = Test s (uncurry (==)) $ map (first f) l

testF2 :: (Show a, Show b, Show c, Eq c) => String -> (a -> b -> c)
       -> [(a, b, c)] -> Test
testF2 s f l = Test s (uncurry (==)) $ map (\(x, y, z) -> (f x y, z))  l

testF3 :: (Show a, Show b, Show c, Show d, Eq d) => String -> (a -> b -> c -> d)
       -> [(a, b, c, d)] -> Test
testF3 s f l = Test s (uncurry (==)) $ map (\(w, x, y, z) ->  (f w x y, z)) l
