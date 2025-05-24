module ParserError where

import Lexer
import Tokens

data ParserError = 
    UnexpectedToken { 
        expectedTkn :: Tkn, 
        received :: Atom Tkn 
    } deriving (Show)