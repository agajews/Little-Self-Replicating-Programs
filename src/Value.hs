{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Value (
    Value(..),
    EvalError(..),
    Thread(..),
    WorldState(..),
    throw,
    pause,
    runThread,
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

type ValueMap = Map.Map Int Value

data EvalError = EvalError

data WorldState = WorldState { univMap :: ValueMap,
                               univSize :: Int,
                               univEdits :: ValueMap,
                               envMap :: ValueMap,
                               randomGen :: StdGen,
                               cellPos :: Int }

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

runThread :: WorldState -> Thread a -> (Either EvalError (Either (Thread a) a), WorldState)
runThread state (Thread t) = unwrapId $ runIdentity $ runStateT (runExceptT $ resume t) state where
    unwrapId (Right (Left (Identity t)), state) = (Right $ Left $ Thread t, state)
    unwrapId (Right (Right x), state) = (Right $ Right x, state)
    unwrapId (Left err, state) = (Left err, state)
