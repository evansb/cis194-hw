module Fun where

data Expr = Lit    Bool
          | Var    String
          | Let    String Expr Expr
          | If     Expr Expr Expr
          | Lambda String Expr
          | App    Expr Expr
            deriving (Eq)


instance Show Expr where
    show (Lit b) = show b
    show (Var v) = v
    show (Let v e1 e2) = "let " ++ v ++ " = " ++ show e1 ++ " in " ++ show e2
    show (If eb e1 e2) = "if " ++ show eb ++
                         " then " ++ show e1 ++
                         " else " ++ show e2
    show (Lambda v e)  = "(\\" ++ v ++ " -> " ++ show e ++ ")"
    show (App e1 e2)   = show e1 ++ " " ++ show e2


data Value = Boolean  Bool
           | Function String Expr
             deriving (Eq)

instance Show Value where
    show (Boolean b) = show b
    show (Function s e) = "(\\" ++ s ++ " -> " ++ show e ++ ")"

type State = String -> Value

empty :: State
empty = const (Boolean False)

extend :: State -> String -> Value -> State
extend st name val = \x -> if x == name then val else st x


subst :: String -> Expr -> Expr -> Expr
subst v e (Lit b)  = Lit b
subst v e (Var v') | v == v'   = e
                   | otherwise = Var v'
subst v e (Let v' e1 e2) | v == v'   = Let v' (subst v e e1) e2
                         | otherwise = Let v' (subst v e e1) (subst v e e2)
subst v e (If e1 e2 e3) = If (subst v e e1)
                          (subst v e e2)
                          (subst v e e3)
subst v e (Lambda v' e1) | v == v'   = Lambda v' e1
                         | otherwise = Lambda v' (subst v e e1)
subst v e (App e1 e2) = App (subst v e e1) (subst v e e2)


evalE :: State -> Expr -> Value
evalE st (Lit b) = Boolean b
evalE st (Var v) = st v
evalE st (Let v e1 e2) = evalE st (subst v e1 e2)
evalE st (If e1 e2 e3) = case evalE st e1 of
                           Function _ _ -> error "not a boolean"
                           Boolean True -> evalE st e2
                           Boolean False -> evalE st e3
evalE st (Lambda v e) = Function v e
evalE st (App e1 e2) = case evalE st e1 of
                         Boolean _ -> error "can't apply a boolean"
                         Function v e -> evalE st (subst v e2 e)

lognot :: Expr
lognot = Lambda "x" $ If (Var "x") (Lit False) (Lit True)

logor :: Expr
logor = Lambda "x" $ Lambda "y" $ If (Var "x") (Lit True) (Var "y")

logand :: Expr
logand = Lambda "x" $ Lambda "y" $ If (Var "x") (Var "y") (Lit False)

logxor :: Expr
logxor = Let "not" lognot $ Lambda "x" $ Lambda "y" $
                   If (Var "x") (Var "not" `App` Var "y") (Var "y")
