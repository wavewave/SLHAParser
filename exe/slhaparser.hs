module Main where

import System.Console.CmdArgs

import HEP.Parser.SLHAParser.ProgType
import HEP.Parser.SLHAParser.Command

main :: IO () 
main = do 
  putStrLn "SLHAParser"
  param <- cmdArgs mode

  commandLineProcess param