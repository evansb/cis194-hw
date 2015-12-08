{-# LANGUAGE OverloadedStrings #-}
module Lec05 where

import Data.ByteString (ByteString)
import Data.Char
import Data.Functor
import Data.Monoid
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.ByteString as BS

-- Unsafe Increment function
inc :: Int -> Int
inc n | n == 5    = unsafePerformIO $ peek nullPtr
      | otherwise = n + 1

-- Capitalize the first letter of a String
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : cs

data Person = Person { fname :: String
                     , lname :: String
                     , age   :: Int
                     }
            deriving (Show)

getName :: IO String
getName = do
  name <- getLine
  return $ capitalize name

-- Main function, asks someone what their name is
main :: IO ()
main = do
  putStrLn "Hi, what's your name?"
  name <- getName
  putStrLn $ "Hello, " ++ name ++ "!"

printFile :: FilePath -> IO ()
printFile fname = do
  file <- BS.readFile fname
  print file

-- Replace all occurences of a ByteString in another ByteString
replace :: ByteString -> ByteString -> ByteString -> ByteString
replace old new bs = pre <> sfx
    where (pre, bs') = BS.breakSubstring old bs
          l = BS.length old
          sfx | BS.isPrefixOf old bs' = new <> replace old new (BS.drop l bs')
              | otherwise             = bs'

rewriteFile :: IO ()
rewriteFile = do
  file <- BS.readFile "note.txt"
  let newfile = replace "Haskell" "Java" file
  BS.writeFile "newnote.txt" newfile

-- Convert a Char to a Word8
c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Convert a Word8 to a Char
w2c :: Word8 -> Char
w2c = chr . fromIntegral

getWords :: IO [ByteString]
getWords = do
  line <- BS.getLine
  return $ BS.split (c2w ' ') line

getWords' :: IO [ByteString]
getWords' = BS.split (c2w ' ') <$> BS.getLine

-- fmap :: Functor f => (a -> b) -> f a -> f b
fmap' :: (a -> b) -> IO a -> IO b
fmap' f ioa = do
  x <- ioa
  return $ f x

-- Get the type of a function
getType :: ByteString -> ByteString -> ByteString
getType func file = unescape $ BS.takeWhile (/= 60) $ BS.drop l bs
    where s = ">" <> func <> "</a> ::  "
          l = BS.length s
          (_, bs) = BS.breakSubstring s file
          unescape = replace "&gt;" ">"

bsToS :: ByteString -> String
bsToS = map w2c . BS.unpack

-- Look up the type of a function
typeOf :: ByteString -> ByteString -> IO ByteString
typeOf modName funcName =
    getType funcName <$> BS.readFile fname
        where fname = bsToS $ replace "." "-" modName <> ".html"

