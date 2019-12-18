This module contains actions that mutate contents of cells and generate new random values. The actions are all in the \texttt{Rand} monad instead of in the \texttt{Thread} monad because they only need the random generator, not any other part of the \texttt{WorldState}, so it would have been overkill to give them an entire \texttt{Thread} coroutine. Also because they're not just used from within the \texttt{Thread} monad, but are also used for initializing the universe at the beginning of execution. 

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
The following is just a helper type alias for the monad we're going to be doing everything in.

\begin{code}
type RandM = Rand StdGen
\end{code}
The following represents the probability that any mutation is going to occur this step.

\begin{code}
mutateP :: Double
mutateP = 0.01
\end{code}
The following represents the probability that the parameter of a \texttt{Lambda}, as opposed to its body, will be mutated.

\begin{code}
mutateParP :: Double
mutateParP = 0.2
\end{code}
The following is the probability that a function, as opposed to its argument, will be mutated in a function call.

\begin{code}
mutateFuncP :: Double
mutateFuncP = 0.3
\end{code}
The following is the probability that an entirely new random value will be generated, as opposed to the differential modifications that are otherwise performed.

\begin{code}
mutateTypeP :: Double
mutateTypeP = 0.1
\end{code}
When an int is mutated, it is randomly incremented or decremented with equal probability.

\begin{code}
mutateInt :: Int -> RandM Int
mutateInt x = do
    b <- getRandom
    return $ if b then x + 1 else x - 1
\end{code}
When a new integer is generated, it is selected from the range $[-5, 5]$.

\begin{code}
randInt :: RandM Int
randInt = getRandomR (-5, 5)
\end{code}

\begin{code}
randIntVal :: RandM Value
randIntVal = randInt >>= return . IntVal
\end{code}
When a new primitive function is generated, it is selected at random from the list of primitive functions.

\begin{code}
randPrimFunc :: RandM Value
randPrimFunc = do
    i <- getRandomR (0, length primFuncs - 1)
    return $ primFuncs !! i
\end{code}
When a new lambda is generated, its parameter is a random integer from the range $[-5, 5]$, and its body is a randomly generated value.

\begin{code}
randLambda :: RandM Value
randLambda = do
    x <- randInt
    v <- randomValue
    return $ Lambda x v
\end{code}
When a variable is generated, it is selected at random from the range $[-5, 5]$.

\begin{code}
randVariable :: RandM Value
randVariable = randInt >>= return . Variable
\end{code}
When a function call is generated, both the function and the argument are randomly generated values. If the function is an integer this will result in the thread crashing, but that's sufficiently low probability that it wouldn't have been worth making a separate random function generator.

\begin{code}
randFuncCall :: RandM Value
randFuncCall = do
    f <- randomValue
    a <- randomValue
    return $ FuncCall f a
\end{code}
The \texttt{mutateInplace} function just puts together all of the above probabilities and mutators and applies them to an arbitrary value.

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
When a new random value is required, its type is selected at random from the 5 different possible types (ints, primitive functions, lambdas, variables, and function calls), with equal probability. A different version of the code could have different probabilities for each of the types, but there are already a lot of parameters to deal with, and it's not clear how these probabilities should deviate from uniform for better performance.

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
The \texttt{mutateValue} function just generates a new random value with probability \texttt{mutateTypeP}, and mutates the existing value otherwise.

\begin{code}
mutateValue :: Value -> RandM Value
mutateValue x = do
    b <- getRandom
    if b < mutateTypeP then randomValue
    else mutateInplace x
\end{code}
When we mutate a thread, we first check whether any mutations are going to occur (probability \texttt{mutateP}), and if so, we pick a random cell to mutate, and then mutate it.

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
