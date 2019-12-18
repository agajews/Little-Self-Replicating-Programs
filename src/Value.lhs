\begin{code}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

\begin{code}
import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Coroutine
\end{code}

\begin{code}
import Control.Monad.Random
import System.Random
\end{code}

\begin{code}
import qualified Data.Map as Map
\end{code}

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

\begin{code}
type ValueMap = Map.Map Int Value
\end{code}

\begin{code}
data EvalError = EvalError
\end{code}

\begin{code}
data WorldState = WorldState { univMap :: ValueMap,
                               univSize :: Int,
                               univEdits :: ValueMap,
                               envMap :: ValueMap,
                               randomGen :: StdGen,
                               cellPos :: Int }
                  deriving (Show)
\end{code}

\begin{code}
newtype Thread a = Thread
    (Coroutine Identity (ExceptT EvalError (StateT WorldState Identity)) a)
    deriving (Functor,
              Applicative,
              Monad)
\end{code}

\begin{code}
instance MonadState WorldState Thread where
    get = Thread $ lift $ get
    put = Thread . lift . put
\end{code}

\begin{code}
throw :: EvalError -> Thread a
throw = Thread . lift . throwError
\end{code}

\begin{code}
pause :: Thread ()
pause = Thread $ suspend $ Identity $ return ()
\end{code}

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

\begin{code}
liftRandom :: Rand StdGen a -> Thread a
liftRandom rand = do
    state <- get
    let (x, g) = runRand rand $ randomGen state
    put $ state { randomGen = g }
    return x
\end{code}
