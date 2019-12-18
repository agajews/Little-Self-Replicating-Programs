\begin{code}
module Builtins (
    primFuncs,
) where
\end{code}

\begin{code}
import Value
import State
import Eval
\end{code}

\begin{code}
primFuncs :: [Value]
primFuncs = [macro3 "if" ifFunc,
             macro2 "define" define,

             func1 "peek" peek,
             func2 "poke" poke,

             func2 "+" $ intOp (+),
             func2 "-" $ intOp (-),
             func2 "*" $ intOp (*),

             func2 ">" $ intBoolOp (>),
             func2 "<" $ intBoolOp (<),
             func2 "=" $ intBoolOp (==),

             func2 "&&" $ boolOp (&&),
             func2 "||" $ boolOp (||),

             func1 "eval" eval,

             func1 "lambda-get-var" lambdaGetVar,
             func1 "lambda-get-val" lambdaGetVal,
             func2 "lambda-set-var" lambdaSetVar,
             func2 "lambda-set-val" lambdaSetVal,

             func1 "funccall-get-func" funcCallGetFunc,
             func1 "funccall-get-arg" funcCallGetArg,
             func2 "funccall-set-func" funcCallSetFunc,
             func2 "funccall-set-arg" funcCallSetArg]
\end{code}

\begin{code}
func1 :: String -> (Value -> Thread Value) -> Value
func1 name f = PrimFunc name $ \x -> do
               x' <- eval x
               f x'
\end{code}

\begin{code}
func2 :: String -> (Value -> Value -> Thread Value) -> Value
func2 name f = PrimFunc name $ \x -> return $
               PrimFunc (name ++ "1") $ \y -> do
               x' <- eval x
               y' <- eval y
               f x' y'
\end{code}

\begin{code}
macro2 :: String -> (Value -> Value -> Thread Value) -> Value
macro2 name f = PrimFunc name $ \x -> return $
                PrimFunc (name ++ "1") $ \y ->
                f x y
\end{code}

\begin{code}
macro3 :: String -> (Value -> Value -> Value -> Thread Value) -> Value
macro3 name f = PrimFunc name $ \x -> return $
                PrimFunc (name ++ "1") $ \y -> return $
                PrimFunc (name ++ "2") $ \z ->
                f x y z
\end{code}

\begin{code}
ifFunc :: Value -> Value -> Value -> Thread Value
ifFunc b thenExpr elseExpr = do
    b' <- eval b
    case b' of
        IntVal x -> if x > 0
            then eval thenExpr
            else eval elseExpr
        _ -> throw EvalError
\end{code}

\begin{code}
define :: Value -> Value -> Thread Value
define (Variable x) y = do
    y' <- eval y
    setVar x y'
    return y'
define _ _ = throw EvalError
\end{code}

\begin{code}
peek :: Value -> Thread Value
peek (IntVal x) = do
    n <- getSize
    y <- getCellPos
    getCell ((x + y) `mod` n)
peek _ = throw EvalError
\end{code}

\begin{code}
poke :: Value -> Value -> Thread Value
poke (IntVal x) val = do
    y <- getCellPos
    n <- getSize
    setCell ((x + y) `mod` n) val
    return val
poke _ _ = throw EvalError
\end{code}

\begin{code}
intOp :: (Int -> Int -> Int) -> Value -> Value -> Thread Value
intOp op (IntVal x) (IntVal y) = return $ IntVal $ op x y
intOp _ _ _ = throw EvalError
\end{code}

\begin{code}
intBoolOp :: (Int -> Int -> Bool) -> Value -> Value -> Thread Value
intBoolOp op (IntVal x) (IntVal y) = return $ IntVal $
    if op x y then 1 else 0
intBoolOp _ _ _ = throw EvalError
\end{code}

\begin{code}
boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Thread Value
boolOp op (IntVal x) (IntVal y) = return $ IntVal $
    if op (x > 0) (y > 0) then 1 else 0
boolOp _ _ _ = throw EvalError
\end{code}

\begin{code}
lambdaGetVar :: Value -> Thread Value
lambdaGetVar (Lambda x _) = return $ Variable x
lambdaGetVar _ = throw EvalError
\end{code}

\begin{code}
lambdaGetVal :: Value -> Thread Value
lambdaGetVal (Lambda _ y) = return y
lambdaGetVal _ = throw EvalError
\end{code}

\begin{code}
lambdaSetVar :: Value -> Value -> Thread Value
lambdaSetVar (Lambda _ y) (Variable x) = return $ Lambda x y
lambdaSetVar _ _ = throw EvalError
\end{code}

\begin{code}
lambdaSetVal :: Value -> Value -> Thread Value
lambdaSetVal (Lambda x _) y = return $ Lambda x y
lambdaSetVal _ _ = throw EvalError
\end{code}

\begin{code}
funcCallGetFunc :: Value -> Thread Value
funcCallGetFunc (FuncCall f _) = return f
funcCallGetFunc _ = throw EvalError
\end{code}

\begin{code}
funcCallGetArg :: Value -> Thread Value
funcCallGetArg (FuncCall _ a) = return a
funcCallGetArg _ = throw EvalError
\end{code}

\begin{code}
funcCallSetFunc :: Value -> Value -> Thread Value
funcCallSetFunc (FuncCall _ a) f = return $ FuncCall f a
funcCallSetFunc _ _ = throw EvalError
\end{code}

\begin{code}
funcCallSetArg :: Value -> Value -> Thread Value
funcCallSetArg (FuncCall f _) a = return $ FuncCall f a
funcCallSetArg _ _ = throw EvalError
\end{code}
