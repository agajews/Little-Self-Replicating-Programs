module State (
    getVar,
    setVar,
    getCell,
    setCell,
    getCellPos,
    setCellPos,
    getSize,
) where

import Value

import Control.Monad.State

import System.Random

import qualified Data.Map as Map

getVar :: Int -> Thread Value
getVar x = do
    state <- get
    case envMap state Map.!? x of
        Just y -> return y
        Nothing -> throw EvalError

setVar :: Int -> Value -> Thread ()
setVar x v = do
    state <- get
    put $ state { envMap = Map.insert x v $ envMap state }

getCell :: Int -> Thread Value
getCell x = do
    state <- get
    return $ univMap state Map.! x

setCell :: Int -> Value -> Thread ()
setCell x v = do
    state <- get
    put $ state { univMap = Map.insert x v $ univMap state }

getCellPos :: Thread Int
getCellPos = do
    state <- get
    return $ cellPos state

setCellPos :: Int -> Thread ()
setCellPos x = do
    state <- get
    put $ state { cellPos = x }

getSize :: Thread Int
getSize = do
    state <- get
    return $ univSize state
