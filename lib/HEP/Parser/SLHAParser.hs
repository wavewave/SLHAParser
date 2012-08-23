{-# LANGUAGE OverloadedStrings #-}

module HEP.Parser.SLHAParser 
( module HEP.Parser.SLHAParser.Type 
, isEndl 
, skipOnlySpace
, line 
, comment
, commentline 
, dataline
, blockline 
, block 
, slha
) where 

import Control.Applicative 
import Control.Monad 
import Data.Attoparsec.ByteString.Char8 
import qualified Data.Attoparsec.Types as P
import Data.ByteString.Char8 hiding (takeWhile,split,map)
-- import Data.List.Split 
import Data.Monoid
-- from this package
import HEP.Parser.SLHAParser.Type 
-- 
import Prelude hiding (takeWhile)


--------------------------
-- some useful predicates
--------------------------

-- | 
isEndl :: Char -> Bool 
isEndl = (== '\n')

-- | 
isSharp :: Char -> Bool 
isSharp = (== '#')

-- | 
isOnlySpace :: Char -> Bool 
isOnlySpace = (== ' ')

----------------------
-- utility parser
----------------------

-- | 
skipOnlySpace :: Parser () 
skipOnlySpace = skipWhile isOnlySpace 

-- | 
stringitem :: Parser String 
stringitem = skipOnlySpace >> liftM unpack (takeWhile1 p)
  where p x =  getAll . mconcat . map All $ 
                 [ not (isSharp x)
                 , not (isSpace x)
                 , not (isEndl x) ] 

----------------------
-- for line parsing
----------------------

-- | 
comment :: Parser Comment 
comment = liftM Comment $ (:) <$> char '#' 
                              <*> fmap unpack (takeWhile (not.isEndl)) 
                              <*  char '\n'
     
-- |       
commentline :: Parser Line 
commentline = CommentLine <$> comment 

-- | 
dataline :: Parser Line 
dataline = do b <- atEnd
              when b $ fail "it's over"
              x <- peekChar 
              case x of 
                Just 'B' -> fail "not dataline, it's block"
                Just '#' -> fail "not dataline, it's comment" 
                _ -> return ()
              DataLine 
                <$> many1 stringitem 
                <*> (skipOnlySpace >> (try comment 
                                       <|> (char '\n' >> return emptyComment)))

-- | 
blockline :: Parser Line 
blockline = 
    BlockLine 
    <$> (blocktag  >>  many1 stringitem)
    <*> (skipOnlySpace >> (try comment 
                           <|> (char '\n' >> return emptyComment)))

-- | 
blocktag :: Parser ()
blocktag = char 'B' >> char 'l' >> char 'o' >> char 'c' >> char 'k' >> return ()

----------------------
-- Complex structure
----------------------
                    
-- | 
block :: Parser Block
block = (:) <$> blockline <*> many1 (try commentline <|> dataline)

-- |
slha :: Parser SLHA
slha = SLHA <$> many commentline <*> many1 block

-- | 
line :: Parser Line 
line = try commentline 
       <|> try blockline 
       <|> dataline


