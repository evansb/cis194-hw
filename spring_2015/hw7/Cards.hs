module Cards ( Deck(..)
             , Card(..)
             , Suit(..)
             , Label(..)
             , suits
             , labels) where

import Data.Vector (Vector)
import qualified Data.Vector as V

type Deck = Vector Card

-- The Card data type
data Card  = Card Label Suit

instance Eq Card where
    Card x _ == Card y _ = x == y

instance Ord Card where
    compare (Card x _) (Card y _) = compare x y

instance Show Card where
    show (Card l s) = show l ++ show s

data Suit  = Spade | Heart | Club | Diamond
             deriving (Eq)

instance Show Suit where
    show Spade   = "\ESC[107;30m\9824 \ESC[0m "
    show Heart   = "\ESC[107;31m\9829 \ESC[0m "
    show Club    = "\ESC[107;30m\9827 \ESC[0m "
    show Diamond = "\ESC[107;31m\9830 \ESC[0m "

data Label = Two   | Three | Four  | Five  | Six   | Seven
           | Eight | Nine  | Ten   | Jack  | Queen | King  | Ace
             deriving (Eq, Ord, Enum)

instance Show Label where
    show l = "\ESC[107;30m " ++ show' l ++ space  ++ "\ESC[0m"
        where
          space | l /= Ten  = " "
                | otherwise = ""
          show' Jack  = "J"
          show' Queen = "Q"
          show' King  = "K"
          show' Ace   = "A"
          show' lbl   = show $ fromEnum lbl + 2

suits :: Vector Suit
suits = V.fromList [Spade, Heart, Club, Diamond]

labels :: Vector Label
labels = V.fromList [Two .. Ace]
