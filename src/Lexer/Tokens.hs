module Tokens where

type Tkn = Token
data Token = OpenBrace  | CloseBrace           | 
           OpenParen    | CloseParen           |
           Keyword Kwrd | Arrow Arrow          |
           Word String  | StringLiteral String |
           Invalid      | EndOfFile            deriving(Show, Eq)

type Kwrd = Keyword
data Keyword = For | In | Case | Let deriving(Show, Eq)

data Arrow = L | R deriving(Show, Eq)