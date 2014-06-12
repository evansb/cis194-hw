
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Employee
import Data.Monoid
import Data.Tree
import System.Environment

glCons :: Employee -> GuestList -> GuestList
glCons empl0 (GL empls0 fun0) = GL (empl0:empls0) (fun0 + empFun empl0)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL empls0 fun0) (GL empls1 fun1) = GL (empls0 ++ empls1) (fun0 + fun1)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node root xs) = f root (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss) where
    withoutBoss = mconcat (map (uncurry moreFun) results)
    withBoss = glCons boss (mconcat (map snd results))

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

parse :: GuestList -> String
parse (GL empl0 fun) = "Fun score : " 
                    ++ show fun ++ "\n"
                    ++ unlines (map empName empl0)
main :: IO ()
main = getArgs 
    >>= readFile . head
    >>= putStrLn . parse . maxFun . read
