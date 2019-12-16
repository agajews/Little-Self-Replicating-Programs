{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rep (
    runStep,
) where

import Control.Monad.Identity
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Coroutine

import System.Random

import qualified Data.Map as Map

data Value = IntVal Int
           | PrimFunc String (Value -> Thread Value)
           | Lambda Int Value
           | Variable Int
           | FuncCall Value Value

-- one local environment for each thread of execution
newtype Env = Env (Map.Map Int Value)

-- a global universe of programs, and a size of the universe
data Univ = Univ { univMap :: UnivMap, univSize :: Int }

type UnivMap = Map.Map Int Value

data EvalError = EvalError

data WorldState = WorldState { universe :: Univ, univEdits :: UnivMap, environment :: Env, randomGen :: StdGen }

newtype Thread a = Thread { unwrapThread :: Coroutine Identity (ExceptT EvalError (StateT WorldState Identity)) a }
    deriving (Functor,
              Applicative,
              Monad)

instance MonadState WorldState Thread where
    get = Thread $ lift $ get
    put = Thread . lift . put

throw :: EvalError -> Thread a
throw = Thread . lift . throwError

pause :: Thread ()
pause = Thread $ suspend $ Identity $ return ()

fromIntVal :: Value -> Int
fromIntVal (IntVal x) = x
fromIntVal _ = undefined

runThread :: WorldState -> Thread a -> (Either EvalError (Either (Thread a) a), WorldState)
runThread state (Thread t) = unwrapId $ runIdentity $ runStateT (runExceptT $ resume t) state where
    unwrapId (Right (Left (Identity t)), state) = (Right $ Left $ Thread t, state)
    unwrapId (Right (Right x), state) = (Right $ Right x, state)
    unwrapId (Left err, state) = (Left err, state)

randRange :: Random a => (a, a) -> Thread a
randRange range = do
    state <- get
    let (x, g) = randomR range (randomGen state)
    put $ state { randomGen = g }
    return x

rand :: Random a => Thread a
rand = do
    state <- get
    let (x, g) = random (randomGen state)
    put $ state { randomGen = g }
    return x

randomThread :: Thread Value
randomThread = do
    state <- get
    let univ = universe state
    let n = univSize univ
    i <- randRange (0, n - 1)
    eval $ univMap univ Map.! i

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

mutateP :: Double
mutateP = 0.01

mutateParP :: Double
mutateParP = 0.2

mutateFuncP :: Double
mutateFuncP = 0.3

mutateTypeP :: Double
mutateTypeP = 0.1

mutate :: Thread ()
mutate = do
    b <- rand
    when (b < mutateP) $ do
        state <- get
        let univ = universe state
        let n = univSize univ
        i <- randRange (0, n - 1)
        let m = univMap univ
        x' <- mutateValue (m Map.! i)
        put $ state { universe = univ { univMap = Map.insert i x' m } }

runStep :: [WorldState] -> [Thread Value] -> ([WorldState], [Thread Value])
runStep states threads = (states'', threads'') where
    (threads', states') = unzip [runThread s (mutate >> t) | (s, t) <- zip states threads]
    restartThread (Left err) = randomThread
    restartThread (Right (Left t)) = t
    restartThread (Right (Right _)) = randomThread
    threads'' = map restartThread threads'
    univ = universe $ head states
    univ' = univ { univMap = Map.union (Map.unions $ map univEdits states') (univMap univ) }
    updateState state = state { universe = univ', univEdits = Map.empty }
    states'' = map updateState states'

getVar :: Int -> Thread Value
getVar x = do
    state <- get
    let (Env env) = environment state
    case env Map.!? x of
        Just y -> return y
        Nothing -> throw EvalError

setVar :: Int -> Value -> Thread ()
setVar x v = do
    state <- get
    let (Env env) = environment state
    put $ state { environment = Env $ Map.insert x v env }

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

primFuncs :: [Value]
primFuncs = []
