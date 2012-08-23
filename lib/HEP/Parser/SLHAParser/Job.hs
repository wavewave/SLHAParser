{-# LANGUAGE OverloadedStrings #-}

module HEP.Parser.SLHAParser.Job where

import Control.Applicative 
import Control.Monad 
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B
-- 
import HEP.Parser.SLHAParser
import HEP.Parser.SLHAParser.Type 

startJob :: IO () 
startJob = do 
  putStrLn "job started"
  str <- B.readFile "log"
  let r = parseOnly slha str 
  print r 
  case r of 
    Left str -> putStrLn str 
    Right result -> do 
      print (length (blocks result))
      mapM_ (\x -> putStrLn "------" >> mapM_ print x) (blocks result)
  -- either putStrLn (mapM print >=> print . length ) r 




-- "# SOFTSUSY SUGRA calculation\n# SOFTSUSY3.3.3 SLHA compliant output\n# B.C. Allanach, Comput. Phys. Commun. 143 (2002) 305-331, hep-ph/0104145\nBlock SPINFO          # Program information\n     1    SOFTSUSY    # spectrum calculator\n     2    3.3.3       # version number\n"