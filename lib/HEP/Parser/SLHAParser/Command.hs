module HEP.Parser.SLHAParser.Command where

import HEP.Parser.SLHAParser.ProgType
import HEP.Parser.SLHAParser.Job

commandLineProcess :: SLHAParser -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
