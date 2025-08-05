module Main where

import Fmm (newApp, runApp)
import System.Console.CmdArgs

main :: IO ()
main = cmdArgs newApp >>= runApp
