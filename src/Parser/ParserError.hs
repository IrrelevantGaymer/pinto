module ParserError where

import Lexer ( Atom )
import Tokens ( Tkn )
data ParserError = 
    UnexpectedToken { 
        expectedTkn :: Tkn, 
        received :: Atom Tkn 
    } deriving (Show)