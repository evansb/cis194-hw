{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

fib:: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = fibgen 0 1 where
    fibgen a b = a : fibgen b (a + b)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
        show  = loop (20::Int)  where
            loop 0  _ = "\n" 
            loop n (Cons x0 rest) = show x0 ++ ", " ++ loop (n - 1) rest

streamToList:: Stream a -> [a]
streamToList (Cons x0 rest) = x0 : streamToList rest

streamRepeat:: a -> Stream a
streamRepeat x0 = Cons x0 (streamRepeat x0)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x0 strm) = Cons (f x0) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x0 = Cons (f x0) (streamFromSeed f (f x0))

nats :: Stream Integer
nats = Cons 0 (streamMap (+1) nats)

interleaveStream :: Stream a -> Stream a -> Stream a
interleaveStream (Cons f frest) (Cons s srest) =
        Cons f (Cons s (interleaveStream srest frest))

streamZip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZip f (Cons n0 rest0) (Cons n1 rest1) = 
        Cons (f n0 n1) (streamZip f rest0 rest1)

zeros:: Stream Integer
zeros = Cons 0 zeros

x:: Stream Integer
x = Cons 0 (Cons 1 zeros)

instance Num (Stream Integer) where
        fromInteger n0 = Cons n0 zeros
        (+) = streamZip (+)
        negate = streamMap (\x0 -> -x0)
    --- AB = (a0 + xA') B
    --     = a0 B + x A'B
    --     = a0 (b0 + xB') + x A'B
    --     = a0.b0 + x (a0.B' + A'B)
        (Cons a0 a') * (Cons b0 b') = 
                Cons (a0 * b0) 
                    (streamMap (* a0) b' +
                        a' * Cons b0 b')

instance Fractional (Stream Integer) where
        (Cons a0 a') / (Cons b0 b') = q where
            tr x0 = floor (fromIntegral x0 / fromIntegral b0::Double)
            hed = floor (fromIntegral a0 / fromIntegral b0::Double)
        --- Q = (a0/b0) + x (A' - QB') magic lol
            q = Cons hed (streamMap tr (a' - (q * b')))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x) --- Holy crap this is awesome

data Matrix = Matrix  Integer Integer 
                      Integer Integer

instance Num Matrix where
        (Matrix a0 b0 c0 d0) * (Matrix a1 b1 c1 d1) = 
            Matrix (a0 * a1 + b0 * c1) (a0 * b1 + b0 * d1) 
                   (c0 * a1 + d0 * c1) (c0 * b1 + d0 * d1)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = let f = Matrix 1 1 
                        1 0 in
         let fn (Matrix _ o _ _) = o
         in fn (f^n)

