This module contains declarations of the basic types that we'll be using throughout the rest of the code. It also contains a few little helper functions that didn't have better homes.
The following language extensions just make things a bit easier, letting us automatically derive a few typeclass instances and implement the \texttt{MonadState} typeclass.

\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
\end{code}

\begin{code}
module Value (
    Value(..),
    EvalError(..),
    Thread(..),
    WorldState(..),
    throw,
    pause,
    runThread,
    liftRandom,
) where
\end{code}
We'll be using transformers to build up the \texttt{Thread} monad, so we need the following imports:

\begin{code}
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Coroutine
\end{code}
We also need the \texttt{Rand} monad to deal with mutations and random initialization. There is a \texttt{RandT} transformer, but since it's just a wrapper for \texttt{StateT} and since we're already using one of those to keep track of the \texttt{WorldState}, I thought it would be more straightforward to add a random generator to the \texttt{WorldState} and make a \texttt{liftRandom} helper function (see below).

\begin{code}
import Control.Monad.Random
import System.Random
\end{code}
For parallelization, we're going make \texttt{NFData} instances for all of the relevant data structures, so that we can force deep enough parallel evaluation at each step of execution.

\begin{code}
import Control.DeepSeq
import Control.Parallel.Strategies
\end{code}
Basically everything is currently implemented with maps, even the universe of cells, which would more reasonably have been implemented as an array. This was just for simplicity. \texttt{Array.Diff} is still experimental, and it would have been annoying to wrap everything in \texttt{ST}  or \texttt{IO}, so I just went with maps for everything. Future versions could use more efficient data structures, but since the point of this project was (1) to be a proof of concept and (2) to try to get a parallelization speedup, it seemed fine to have the sequential code be a bit inefficient.

\begin{code}
import qualified Data.Map as Map
\end{code}
The \texttt{Value} type represents both code and data in our small interpreted language. It only has support for integers and functions, and the functions are all fexprs for simplicity.

\begin{code}
data Value = IntVal Int
           | PrimFunc String (Value -> Thread Value)
           | Lambda Int Value
           | Variable Int
           | FuncCall Value Value
\end{code}

\begin{code}
instance Show Value where
    show (IntVal x) = show x
    show (PrimFunc name _) = name
    show (Lambda var val) =
        "(lambda var:" ++ show var ++ " " ++ show val ++ ")"
    show (Variable var) = "var:" ++ show var
    show (FuncCall f a) = "(" ++ show f ++ " " ++ show a ++ ")"
\end{code}
The following is just a helpful type alias, because there are lots of maps from integers to values.

\begin{code}
type ValueMap = Map.Map Int Value
\end{code}
The following is our error type, which causes the evaluation of a thread to halt prematurely. A different version of this code could have different \texttt{EvalError} constructors to allow for easier inspection of what the code is doing, but for this first version I went with a single constructor.

\begin{code}
data EvalError = EvalError
\end{code}
The \texttt{WorldState} type contains all of the state data a single thread of execution needs in order to operate. The \texttt{univMap} is a read-only map from cell-number to value (which should be thought of as an array), and is the same across all threads. The \texttt{univSize} field is the size of the universe, i.e.\ the number of cells. The \texttt{univEdits} map contains the current thread's edits to the universe since the last time the different \texttt{univMap}s have been synchronized. When a cell's value is queried, it is first searched for in \texttt{univEdits}, and then in \texttt{univMap}. When a cell's value is written to, it is written in \texttt{univEdits}. The \texttt{envMap} contains the current thread's local scope. This is unique to each thread. This is mainly used for arguments and local variables. The \texttt{randomGen} field is the random generator for the current thread. The generators for different threads are initialized with different random seeds. Finally, the location of the cell the thread is currently evaluating is stored in \texttt{cellPos}.

\begin{code}
data WorldState = WorldState { univMap :: ValueMap,
                               univSize :: Int,
                               univEdits :: ValueMap,
                               envMap :: ValueMap,
                               randomGen :: StdGen,
                               cellPos :: Int,
                               evalTime :: Int }
                  deriving (Show)
\end{code}
A \texttt{Thread} is an identity coroutine (meaning it can be paused, but doesn't generate a value until it's finished) that can fail with an \texttt{EvalError}, and always has a \texttt{WorldState}, even if it has failed.

\begin{code}
newtype Thread a = Thread
    (Coroutine Identity (ExceptT EvalError (StateT WorldState Identity)) a)
    deriving (Functor,
              Applicative,
              Monad)
\end{code}
The following is a helper instance making it easier to access the internal \texttt{WorldState}.

\begin{code}
instance MonadState WorldState Thread where
    get = Thread $ lift $ get
    put = Thread . lift . put
\end{code}
And as promised, here are the relevant \texttt{NFData} instances:

\begin{code}
instance NFData (Thread a) where
    rnf t = seq t ()

instance NFData EvalError where
    rnf e = seq e ()

instance NFData Value where
    rnf (IntVal x) = seq x ()
    rnf (PrimFunc name f) = seq name $ seq f ()
    rnf (Lambda var val) = seq var $ deepseq val ()
    rnf (Variable var) = seq var ()
    rnf (FuncCall f a) = deepseq f $ deepseq a ()

instance NFData WorldState where
    rnf (WorldState { univMap,
                      univSize,
                      univEdits,
                      envMap,
                      randomGen,
                      cellPos,
                      evalTime }) = runEval $ do
        rdeepseq univMap
        rseq univSize
        rdeepseq univEdits
        rdeepseq envMap
        rseq randomGen
        rseq cellPos
        rseq evalTime
        return ()
\end{code}
We don't need a \texttt{MonadError} instance since we never need to catch any errors, so this is essentially just the \texttt{throwError} method from the \texttt{MonadError} typeclass.

\begin{code}
throw :: EvalError -> Thread a
throw = Thread . lift . throwError
\end{code}
As the name suggests, the \texttt{pause} function pauses the current thread.

\begin{code}
pause :: Thread ()
pause = Thread $ suspend $ Identity $ return ()
\end{code}
The \texttt{runThread} function just runs the whole monad transformer and gets it into a form we can work with directly. This is done at every step of execution.

\begin{code}
type Unwrapped a = (Either EvalError (Either (Thread a) a), WorldState)

runThread :: WorldState -> Thread a -> Unwrapped a
runThread state (Thread t) =
    unwrapId . runIdentity . flip runStateT state . runExceptT . resume $ t
    where
        unwrapId (Right (Left (Identity t)), s) = (Right $ Left $ Thread t, s)
        unwrapId (Right (Right x), s) = (Right $ Right x, s)
        unwrapId (Left err, s) = (Left err, s)
\end{code}
The following is just a helper function that lifts an action from the \texttt{Rand} monad to the \texttt{Thread} monad.

\begin{code}
liftRandom :: Rand StdGen a -> Thread a
liftRandom rand = do
    state <- get
    let (x, g) = runRand rand $ randomGen state
    put $ state { randomGen = g }
    return x
\end{code}
