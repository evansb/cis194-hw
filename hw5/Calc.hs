{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

import ExprT 
import Parser
import StackVM
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (ExprT.Lit int) = int
eval (ExprT.Add int0 int1) = (+) (eval int0) (eval int1)
eval (ExprT.Mul int0 int1) = (*) (eval int0) (eval int1)

evalStr :: String -> Maybe Integer
evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
                  (Just exprT) -> Just (eval exprT)
                  Nothing    -> Nothing

class Expr a where
        mul:: a -> a -> a 
        lit:: Integer -> a
        add:: a -> a -> a 

instance Expr ExprT where
        expr1 `mul` expr2 = ExprT.Mul expr1 expr2
        expr1 `add` expr2 = ExprT.Add expr1 expr2
        lit = ExprT.Lit

instance Expr Integer where
        mul = (*)
        add = (+)
        lit = id

instance Expr Bool where
        mul = (&&)
        add = (||)
        lit = (>0)

newtype MinMax = MinMax Integer deriving (Eq,Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
        (MinMax int0) `mul` (MinMax int1) = MinMax (if int0 < int1 then int0 else int1)
        (MinMax int0) `add` (MinMax int1) = MinMax (if int0 > int1 then int0 else int1)
        lit = MinMax

instance Expr Mod7 where
        mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
        add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
        lit = Mod7

instance Expr Program where
        add e1 e2 = e1 ++ e2 ++ [StackVM.Add]
        mul e1 e2 = e1 ++ e2 ++ [StackVM.Mul]
        lit e1 = [PushI e1] 

compile:: String -> Maybe Program
compile str = parseExp lit add mul str :: Maybe Program

class HasVars a where
        var :: String -> a

data VarExprT = Lit Integer
                | Var String
                | Add VarExprT VarExprT
                | Mul VarExprT VarExprT

instance HasVars (M.Map String Integer -> Maybe Integer) where
        var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
        lit int0 _ = Just int0
        add var0 var1 map0 = do int0 <- var0 map0
                                int1 <- var1 map0
                                return (int0 + int1)
        mul var0 var1 map0 = do int0 <- var0 map0
                                int1 <- var1 map0
                                return (int0 * int1)

withVars :: [(String, Integer)] 
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs ex = ex $ M.fromList vs

main :: IO ()
main = do 
          print $ withVars [("x", 6)] $ add (lit 3) (var "x")
          print $ withVars [("x", 6)] $ add (lit 3) (var "y")
          print $ withVars  [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))
