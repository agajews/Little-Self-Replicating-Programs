module Rep (
    runStep,
) where

import Value
import State
import Eval
import Builtins

import Control.Monad

import qualified Data.Map as Map

mutateP :: Double
mutateP = 0.01

mutateParP :: Double
mutateParP = 0.2

mutateFuncP :: Double
mutateFuncP = 0.3

mutateTypeP :: Double
mutateTypeP = 0.1

randomThread :: Thread Value
randomThread = do
    n <- getSize
    i <- randRange (0, n - 1)
    setCellPos i
    cell <- getCell i
    eval cell

mutateInt :: Int -> Thread Int
mutateInt x = do
    b <- rand
    return $ if b then x + 1 else x - 1

randInt :: Thread Int
randInt = randRange (-5, 5)

randIntVal :: Thread Value
randIntVal = randInt >>= return . IntVal

randPrimFunc :: Thread Value
randPrimFunc = do
    i <- randRange (0, length primFuncs - 1)
    return $ primFuncs !! i

randLambda :: Thread Value
randLambda = do
    x <- randInt
    v <- randomValue
    return $ Lambda x v

randVariable :: Thread Value
randVariable = randInt >>= return . Variable

randFuncCall :: Thread Value
randFuncCall = do
    f <- randomValue
    a <- randomValue
    return $ FuncCall f a

mutateInplace :: Value -> Thread Value
mutateInplace (IntVal x) = mutateInt x >>= return . IntVal
mutateInplace (PrimFunc _ _) = randPrimFunc
mutateInplace (Lambda x v) = do
    b <- rand
    if b < mutateParP then do
        x' <- mutateInt x
        return $ Lambda x' v
    else do
        v' <- mutateInplace v
        return $ Lambda x v'
mutateInplace (Variable x) = mutateInt x >>= return . Variable
mutateInplace (FuncCall f a) = do
    b <- rand
    if b < mutateFuncP then do
        f' <- mutateInplace f
        return $ FuncCall f' a
    else do
        a' <- mutateInplace a
        return $ FuncCall f a'

randomValue :: Thread Value
randomValue = do
    b <- randRange (0, 4)
    case b :: Int of
        0 -> randIntVal
        1 -> randPrimFunc
        2 -> randLambda
        3 -> randVariable
        4 -> randFuncCall

mutateValue :: Value -> Thread Value
mutateValue x = do
    b <- rand
    if b < mutateTypeP then randomValue
    else mutateInplace x

mutate :: Thread ()
mutate = do
    b <- rand
    when (b < mutateP) $ do
        n <- getSize
        i <- randRange (0, n - 1)
        x <- getCell i
        x' <- mutateValue x
        setCell i x'

runStep :: [WorldState] -> [Thread Value] -> ([WorldState], [Thread Value])
runStep states threads = (states'', threads'') where
    (threads', states') = unzip [runThread s (mutate >> t) | (s, t) <- zip states threads]
    restartThread (Left err) = randomThread
    restartThread (Right (Left t)) = t
    restartThread (Right (Right _)) = randomThread
    threads'' = map restartThread threads'
    univ = univMap $ head states
    univ' = Map.union (Map.unions $ map univEdits states') univ 
    updateState state = state { univMap = univ', univEdits = Map.empty }
    states'' = map updateState states'
