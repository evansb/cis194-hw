
-- This is CIS194 Homework 1
-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf
-- Evan Sebastian (evans@comp.nus.edu.sg)

-- Part One : Validating Credit Card Numbers

-- toDigits convert an integer m to a list of its digits
toDigits:: Integer -> [Integer]
toDigits m = loop m [] where
    loop x y | x <= 0    = y
             | otherwise = loop (x `div` 10) ((x `mod` 10) : y)

-- toDigitsRev return the reversed digit (see toDigits)
toDigitsRev:: Integer -> [Integer]
toDigitsRev = reverse . toDigits

-- doubleEveryOther multiplies each 2nd element by 2,
-- starting from the right.
doubleEveryOther:: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse  where
    oneTwo = 1 : 2 : oneTwo

-- sumDigits reduces a list of integers to a sum of its digits
sumDigits:: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

-- validate validates a credit card number
validate:: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

-- Part 2 : Tower of Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi:: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
-- To move n disc from a to b using c as temp,
hanoi n a b c = hanoi (n - 1) a c b ++  -- Move n - 1 disc from a to c using b as temp
                [(a, b)] ++             -- Move the top disc from a to b 
                hanoi (n - 1) c b a     -- Move n - 1 disc from c to b using a as temp
-- Was it that hard?
