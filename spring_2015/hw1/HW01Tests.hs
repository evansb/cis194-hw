-- CIS 194, Spring 2015
--
-- Test cases for HW 01

module HW01Tests where

import HW01
import Testing

-- Exercise 1 -----------------------------------------

testLastDigit :: (Integer, Integer) -> Bool
testLastDigit (n, d) = lastDigit n == d

testDropLastDigit :: (Integer, Integer) -> Bool
testDropLastDigit (n, d) = dropLastDigit n == d

test :: (Eq a, Show a) => String -> (b -> a) -> [(b, a)] -> [Test]
test name f = map (\io -> Test name ((snd io) ==) (map f[fst io]))

ex1Tests :: [Test]
ex1Tests = [ Test "lastDigit test" testLastDigit
             [(123, 3), (1234, 4), (5, 5), (10, 0), (0, 0)]
           , Test "dropLastDigit test" testDropLastDigit
             [(123, 12), (1234, 123), (5, 0), (10, 1), (0,0)]
           ]

-- Exercise 2 -----------------------------------------

ex2Tests :: [Test]
ex2Tests = test "toRevDigits" toRevDigits
  [(4321, [1,2,3,4]), (1, [1])]

-- Exercise 3 -----------------------------------------

ex3Tests :: [Test]
ex3Tests = test "doubleEveryOther" doubleEveryOther
  [ ([1], [1])
  , ([1,2,3,4], [1,4,3,8])
  , ([1,2,3], [1,4,3])
  , ([], []) ]

-- Exercise 4 -----------------------------------------

ex4Tests :: [Test]
ex4Tests = test "sumDigits" sumDigits
  [ ([], 0)
  , ([1, 2], 3)
  , ([11, 2], 4)
  , ([10, 5, 18, 4], 19) ]

-- Exercise 5 -----------------------------------------

ex5Tests :: [Test]
ex5Tests = test "luhn" luhn
  [ (5594589764218858, True),
    (1234567898765432, False) ]

-- Exercise 6 -----------------------------------------

ex6Tests :: [Test]
ex6Tests = [
  Test "hanoi test" (==  [("a","c"), ("a","b"), ("c","b")])
    [hanoi 2 "a" "b" "c"] ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , ex5Tests
                  , ex6Tests
                  ]
