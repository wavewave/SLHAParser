{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HEP.Parser.SLHAParser.Type where

newtype Comment = Comment { unComment :: String } 
    deriving Eq 

instance Show Comment where 
  show (Comment cmt) = show cmt 

-- |
emptyComment :: Comment 
emptyComment = Comment ""

-- | 
data Line = CommentLine Comment 
          | DataLine  [String] Comment 
          | BlockLine [String] Comment 
            deriving (Show,Eq)

type Block = [Line] 

type Header = [Line]

data SLHA = SLHA { header :: Header 
                 , blocks :: [Block] }
          deriving (Show)



isBlockLine :: Line -> Bool 
isBlockLine (BlockLine _ _) = True 
isBlockLine _ = False 