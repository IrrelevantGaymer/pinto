module Tokens where

type Tkn = Token
data Token = OpenBrace  | CloseBrace           | 
           OpenParen    | CloseParen           |
           Keyword Kwrd | Arrow Arrow          |
           Word String  | StringLiteral String |
           Invalid deriving(Show)

type Kwrd = Keyword
data Keyword = For | In | Case | Let deriving(Show)

data Arrow = L | R deriving(Show)