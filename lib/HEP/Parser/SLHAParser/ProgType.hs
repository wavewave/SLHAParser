{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Parser.SLHAParser.ProgType where 

import System.Console.CmdArgs

data SLHAParser = Test 
              deriving (Show,Data,Typeable)

test :: SLHAParser
test = Test 

mode = modes [test]

