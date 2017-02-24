
module Main where

import Agda.Main (runAgda)
import Agda.Compiler.UHC.Compiler

main :: IO ()
main = runAgda [uhcBackend]

