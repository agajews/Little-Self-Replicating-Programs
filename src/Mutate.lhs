\begin{code}
module Mutate (
    mutate,
    randomValue,
) where
\end{code}

\begin{code}
import Value
import State
import Builtins
\end{code}

\begin{code}
import System.Random
import Control.Monad.Random
\end{code}

\begin{code}
type RandM = Rand StdGen
\end{code}

\begin{code}
mutateP :: Double
mutateP = 0.01
\end{code}

\begin{code}
mutateParP :: Double
mutateParP = 0.2
\end{code}

\begin{code}
mutateFuncP :: Double
mutateFuncP = 0.3
\end{code}

\begin{code}
mutateTypeP :: Double
mutateTypeP = 0.1
\end{code}

\begin{code}
mutateInt :: Int -> RandM Int
mutateInt x = do
    b <- getRandom
    return $ if b then x + 1 else x - 1
\end{code}

\begin{code}
randInt :: RandM Int
randInt = getRandomR (-5, 5)
\end{code}

\begin{code}
randIntVal :: RandM Value
randIntVal = randInt >>= return . IntVal
\end{code}

\begin{code}
randPrimFunc :: RandM Value
randPrimFunc = do
    i <- getRandomR (0, length primFuncs - 1)
    return $ primFuncs !! i
\end{code}

\begin{code}
randLambda :: RandM Value
randLambda = do
    x <- randInt
    v <- randomValue
    return $ Lambda x v
\end{code}

\begin{code}
randVariable :: RandM Value
randVariable = randInt >>= return . Variable
\end{code}

\begin{code}
randFuncCall :: RandM Value
randFuncCall = do
    f <- randomValue
    a <- randomValue
    return $ FuncCall f a
\end{code}

\begin{code}
mutateInplace :: Value -> RandM Value
mutateInplace (IntVal x) = mutateInt x >>= return . IntVal
mutateInplace (PrimFunc _ _) = randPrimFunc
mutateInplace (Lambda x v) = do
    b <- getRandom
    if b < mutateParP then do
        x' <- mutateInt x
        return $ Lambda x' v
    else do
        v' <- mutateInplace v
        return $ Lambda x v'
mutateInplace (Variable x) = mutateInt x >>= return . Variable
mutateInplace (FuncCall f a) = do
    b <- getRandom
    if b < mutateFuncP then do
        f' <- mutateInplace f
        return $ FuncCall f' a
    else do
        a' <- mutateInplace a
        return $ FuncCall f a'
\end{code}

\begin{code}
randomValue :: RandM Value
randomValue = do
    b <- getRandomR (0, 4)
    case b :: Int of
        0 -> randIntVal
        1 -> randPrimFunc
        2 -> randLambda
        3 -> randVariable
        4 -> randFuncCall
\end{code}

\begin{code}
mutateValue :: Value -> RandM Value
mutateValue x = do
    b <- getRandom
    if b < mutateTypeP then randomValue
    else mutateInplace x
\end{code}

\begin{code}
mutate :: Thread ()
mutate = do
    b <- liftRandom getRandom
    when (b < mutateP) $ do
        n <- getSize
        i <- liftRandom $ getRandomR (0, n - 1)
        x <- getCell i
        x' <- liftRandom $ mutateValue x
        setCell i x'
\end{code}
