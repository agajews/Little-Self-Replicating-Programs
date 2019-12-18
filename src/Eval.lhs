\begin{code}
module Eval (
    eval,
) where
\end{code}

\begin{code}
import Value
import State
\end{code}

\begin{code}
eval :: Value -> Thread Value
eval x@(IntVal _) = return x
eval x@(PrimFunc _ _) = return x
eval x@(Lambda _ _) = return x
eval (Variable x) = getVar x
eval (FuncCall f a) = do
    f' <- eval f
    case f' of
        PrimFunc _ g -> do
            y <- g a
            pause
            return y
        Lambda x v -> do
            setVar x a
            y <- eval v
            pause
            return y
        _ -> throw EvalError
\end{code}