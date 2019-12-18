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
import qualified Data.Map as Map
\end{code}

\begin{code}
randomThread :: Thread Value
randomThread = do
    n <- getSize
    i <- liftRandom $ getRandomR (0, n - 1)
    setCellPos i
    cell <- getCell i
    eval cell
\end{code}

\begin{code}
runStep :: ([WorldState], [Thread Value]) -> ([WorldState], [Thread Value])
runStep (states, threads) = (states'', threads'') where
    (threads', states') = unzip
        [runThread s (mutate >> t) | (s, t) <- zip states threads]
    restartThread (Left err) = randomThread
    restartThread (Right (Left t)) = t
    restartThread (Right (Right _)) = randomThread
    threads'' = map restartThread threads'
    univ = univMap $ head states
    univ' = Map.union (Map.unions $ map univEdits states') univ 
    updateState state = state { univMap = univ', univEdits = Map.empty }
    states'' = map updateState states'
\end{code}

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
                               cellPos = 0 }
    states = [makeState s | s <- seeds]
    threads = replicate nThreads randomThread
\end{code}

\begin{code}
runN :: Int -> Int -> Int -> Int -> [WorldState]
runN nCells nThreads seed n =
    fst $ iterate runStep (initialize nCells nThreads seed) !! n
\end{code}
