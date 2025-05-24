module Tokens where

type Tkn = Token
data Token = OpenBrace  | CloseBrace           | 
           OpenParen    | CloseParen           |
           OpenBracket  | CloseBracket         |
           Keyword Kwrd | Arrow Arrow          |
           Word String  | StringLiteral String |
           Assign       | Invalid              |
           EndOfFile    deriving(Show, Eq)

type Kwrd = Keyword
data Keyword = For | In | Case | Let | Start | With deriving(Show, Eq)

data Arrow = L | R deriving(Show, Eq)