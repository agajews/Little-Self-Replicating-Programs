This module contains some helper functions for dealing with the \texttt{WorldState}.

\begin{code}
module State (
    getVar,
    setVar,
    getCell,
    setCell,
    getCellPos,
    setCellPos,
    getSize,
    resetEvalTime,
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
The \texttt{getVar} function just gets a variable from the local execution scope, or throws an error if it's not found.

\begin{code}
getVar :: Int -> Thread Value
getVar x = do
    state <- get
    case envMap state Map.!? x of
        Just y -> return y
        Nothing -> throw EvalError
\end{code}
The \texttt{setVar} function just sets a variable in the local execution scope.

\begin{code}
setVar :: Int -> Value -> Thread ()
setVar x v = do
    state <- get
    put $ state { envMap = Map.insert x v $ envMap state }
\end{code}
The \texttt{getCell} function just gets the value of a cell from the universe, looking first in the current thread's edits and then in the read-only \texttt{univMap}. It causes the program to crash if the requested cell is out of bounds.

\begin{code}
getCell :: Int -> Thread Value
getCell x = do
    state <- get
    return $ case univEdits state Map.!? x of
        Just y -> y
        Nothing -> univMap state Map.! x
\end{code}
The \texttt{setCell} function just sets the value of a cell in the thread's local \texttt{univEdits}.

\begin{code}
setCell :: Int -> Value -> Thread ()
setCell x v = do
    state <- get
    put $ state { univEdits = Map.insert x v $ univMap state }
\end{code}
The following just gets the index of the cell the current thread is evaluating. This is used for some of the locality-sensitive builtin functions.

\begin{code}
getCellPos :: Thread Int
getCellPos = do
    state <- get
    return $ cellPos state
\end{code}
The following just sets the index of the cell the current thread is evaluating.

\begin{code}
setCellPos :: Int -> Thread ()
setCellPos x = do
    state <- get
    put $ state { cellPos = x }
\end{code}
The following just gets the size of the universe, i.e.\ the total number of cells.

\begin{code}
getSize :: Thread Int
getSize = do
    state <- get
    return $ univSize state
\end{code}
The following just resets the \texttt{evalTime} to 0. It is called when a thread starts evaluating a new cell.

\begin{code}
resetEvalTime :: Thread ()
resetEvalTime = do
    state <- get
    put $ state { evalTime = 0 }
\end{code}
