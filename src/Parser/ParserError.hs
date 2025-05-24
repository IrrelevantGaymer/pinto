module ParserError where

import Lexer ( Atom )
import Tokens ( Tkn )

{-
 - TODO: implement a FullResult sumtype that stores
 - soft or hard errors, for easier error handling.
 - We could differentiate between "Oh we tried to parse this but it didn't work"
 - versus "We tried to parse this, and it was supposed ot be this but... 
 - there was an error"
 -}
data ParserError = 
    UnexpectedToken { 
        expectedTkn :: Tkn, 
        received :: Atom Tkn 
    } |
    ExpectedWord {
        received :: Atom Tkn  
    } |
    Empty deriving (Show)
    