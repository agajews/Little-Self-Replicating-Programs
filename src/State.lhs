\begin{code}
module State (
    getVar,
    setVar,
    getCell,
    setCell,
    getCellPos,
    setCellPos,
    getSize,
) where
\end{code}

\begin{code}
import Value
\end{code}

\begin{code}
import Control.Monad.State
\end{code}

\begin{code}
import System.Random
\end{code}

\begin{code}
import qualified Data.Map as Map
\end{code}

\begin{code}
getVar :: Int -> Thread Value
getVar x = do
    state <- get
    case envMap state Map.!? x of
        Just y -> return y
        Nothing -> throw EvalError
\end{code}

\begin{code}
setVar :: Int -> Value -> Thread ()
setVar x v = do
    state <- get
    put $ state { envMap = Map.insert x v $ envMap state }
\end{code}

\begin{code}
getCell :: Int -> Thread Value
getCell x = do
    state <- get
    return $ univMap state Map.! x
\end{code}

\begin{code}
setCell :: Int -> Value -> Thread ()
setCell x v = do
    state <- get
    put $ state { univMap = Map.insert x v $ univMap state }
\end{code}

\begin{code}
getCellPos :: Thread Int
getCellPos = do
    state <- get
    return $ cellPos state
\end{code}

\begin{code}
setCellPos :: Int -> Thread ()
setCellPos x = do
    state <- get
    put $ state { cellPos = x }
\end{code}

\begin{code}
getSize :: Thread Int
getSize = do
    state <- get
    return $ univSize state
\end{code}
