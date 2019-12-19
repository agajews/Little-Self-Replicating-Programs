module Main where

import Rep

main :: IO ()
main = print (runN 100 100 42 10000)
