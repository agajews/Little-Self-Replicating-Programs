This module contains the heart and soul of the simulator, the code that sets up the initial state, and the code that takes a state and runs one step of simulation on it. That is, this module contains the initial value and the update rule of the dynamical system we're building.

\begin{code}
module Rep (
    runStep,
    runN,
) where
\end{code}

\begin{code}
import Value
import State
import Eval
import Mutate
\end{code}

\begin{code}
import Control.Monad.Random
\end{code}

\begin{code}
import Control.Parallel.Strategies
\end{code}

\begin{code}
import qualified Data.Map as Map
\end{code}
The \texttt{randomThread} function assigns a thread to a random cell to evaluate, and starts it evaluating that random cell.

\begin{code}
randomThread :: Thread Value
randomThread = do
    n <- getSize
    i <- liftRandom $ getRandomR (0, n - 1)
    setCellPos i
    resetEvalTime
    cell <- getCell i
    eval cell
\end{code}
To run a step of simulation, we tell the threads to mutate the universe, and then we run the threads for one step of execution, randomly restart any threads that have finished evaluating their assigned cells, then merge edits to the universe and write the results back to the thread states. In terms of parallelization, it is relatively straightforward, just doing a parallel map over the \texttt{runThread}s.

\begin{code}
runStep :: ([WorldState], [Thread Value]) -> ([WorldState], [Thread Value])
runStep (states, threads) = (states'', threads'') where
    evalList = [runThread s (mutate >> t) | (s, t) <- zip states threads]
    (threads', states') = unzip (evalList `using` parList rdeepseq)
    restartThread (Left err) = randomThread
    restartThread (Right (Left t)) = t
    restartThread (Right (Right _)) = randomThread
    threads'' = map restartThread threads'
    univ = univMap $ head states
    univ' = Map.union (Map.unions $ map univEdits states') univ 
    updateState state = state { univMap = univ',
                                univEdits = Map.empty,
                                evalTime = evalTime state + 1 }
    states'' = map updateState states'
\end{code}
At the beginning of the simulation, we set each cell to a random value, generate some random seeds to give each thread a different random generator, and start each thread on evaluating a random cell.

\begin{code}
initialize :: Int -> Int -> Int -> ([WorldState], [Thread Value])
initialize nCells nThreads seed = (states, threads) where
    rand = do
        cells <- sequence $ replicate nCells randomValue
        seeds <- sequence $ replicate nThreads getRandom
        return (cells, seeds)
    (cells, seeds) = evalRand rand $ mkStdGen seed
    univ = Map.fromList $ zip [0..] cells
    makeState s = WorldState { univMap = univ,
                               univSize = nCells,
                               univEdits = Map.empty,
                               envMap = Map.empty,
                               randomGen = mkStdGen s,
                               cellPos = 0,
                               evalTime = 0 }
    states = [makeState s | s <- seeds]
    threads = replicate nThreads randomThread
\end{code}
The following is just a helper function that will run the simulation for $n$ steps, and output the longest evaluation times for each thread.

\begin{code}
runN :: Int -> Int -> Int -> Int -> [Int]
runN nCells nThreads seed n =
    runN' (initialize nCells nThreads seed) (replicate nThreads 0) n where
        runN' state maxs 0 = maxs
        runN' state maxs m = runN' (states, threads) maxs' (m - 1) where
            (states, threads) = runStep state
            maxs' = [max (evalTime s) m | (s, m) <- zip states maxs]
\end{code}
