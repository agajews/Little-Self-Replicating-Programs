module RepOld (
    runStep,
    runN,
) where

import Value
import State
import Eval
import Mutate

import Control.Monad.Random

import qualified Data.Map as Map

randomThread :: Thread Value
randomThread = do
    n <- getSize
    i <- liftRandom $ getRandomR (0, n - 1)
    setCellPos i
    cell <- getCell i
    eval cell

runStep :: ([WorldState], [Thread Value]) -> ([WorldState], [Thread Value])
runStep (states, threads) = (states'', threads'') where
    (threads', states') = unzip [runThread s (mutate >> t) | (s, t) <- zip states threads]
    restartThread (Left err) = randomThread
    restartThread (Right (Left t)) = t
    restartThread (Right (Right _)) = randomThread
    threads'' = map restartThread threads'
    univ = univMap $ head states
    univ' = Map.union (Map.unions $ map univEdits states') univ 
    updateState state = state { univMap = univ', univEdits = Map.empty }
    states'' = map updateState states'

initialize :: Int -> Int -> Int -> ([WorldState], [Thread Value])
initialize nCells nThreads seed = (states, threads) where
    rand = do
        cells <- sequence $ replicate nCells randomValue
        seeds <- sequence $ replicate nThreads getRandom
        return (cells, seeds)
    (cells, seeds) = evalRand rand $ mkStdGen seed
    univ = Map.fromList $ zip [0..] cells
    states = [WorldState { univMap = univ,
                           univSize = nCells,
                           univEdits = Map.empty,
                           envMap = Map.empty,
                           randomGen = mkStdGen s,
                           cellPos = 0 } | s <- seeds]
    threads = replicate nThreads randomThread

runN :: Int -> Int -> Int -> Int -> [WorldState]
runN nCells nThreads seed n = fst $ iterate runStep (initialize nCells nThreads seed) !! n
