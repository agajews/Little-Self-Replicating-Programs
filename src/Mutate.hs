module Mutate (
    mutate,
    randomValue,
) where

import Value
import State
import Builtins

import System.Random
import Control.Monad.Random

type RandM = Rand StdGen

mutateP :: Double
mutateP = 0.01

mutateParP :: Double
mutateParP = 0.2

mutateFuncP :: Double
mutateFuncP = 0.3

mutateTypeP :: Double
mutateTypeP = 0.1

mutateInt :: Int -> RandM Int
mutateInt x = do
    b <- getRandom
    return $ if b then x + 1 else x - 1

randInt :: RandM Int
randInt = getRandomR (-5, 5)

randIntVal :: RandM Value
randIntVal = randInt >>= return . IntVal

randPrimFunc :: RandM Value
randPrimFunc = do
    i <- getRandomR (0, length primFuncs - 1)
    return $ primFuncs !! i

randLambda :: RandM Value
randLambda = do
    x <- randInt
    v <- randomValue
    return $ Lambda x v

randVariable :: RandM Value
randVariable = randInt >>= return . Variable

randFuncCall :: RandM Value
randFuncCall = do
    f <- randomValue
    a <- randomValue
    return $ FuncCall f a

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

randomValue :: RandM Value
randomValue = do
    b <- getRandomR (0, 4)
    case b :: Int of
        0 -> randIntVal
        1 -> randPrimFunc
        2 -> randLambda
        3 -> randVariable
        4 -> randFuncCall

mutateValue :: Value -> RandM Value
mutateValue x = do
    b <- getRandom
    if b < mutateTypeP then randomValue
    else mutateInplace x

mutate :: Thread ()
mutate = do
    b <- liftRandom getRandom
    when (b < mutateP) $ do
        n <- getSize
        i <- liftRandom $ getRandomR (0, n - 1)
        x <- getCell i
        x' <- liftRandom $ mutateValue x
        setCell i x'
