module Main where

import Rep

main :: IO ()
main = print (runN 1000 10 42 1)
