This module is very short, and only exists because it can't go anywhere else. It only contains one function, the \texttt{eval} function, which evaluates a value in the current thread.

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
\end{code}
Values are all autoquoted, as in Lisp:

\begin{code}
eval x@(IntVal _) = return x
eval x@(PrimFunc _ _) = return x
eval x@(Lambda _ _) = return x
\end{code}
Evaluating a variable just gets it from the local environment:

\begin{code}
eval (Variable x) = getVar x
\end{code}
Evaluating a function is the only time the current thread gets paused, and it gets paused between when the result is evaluated and when it is returned. Attempting to evaluate something that isn't a function kills the thread.

\begin{code}
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
