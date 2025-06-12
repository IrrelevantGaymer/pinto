module Lexer.Tokens where {
    type Tkn = Token;
    data Token = OpenBrace             | CloseBrace           | 
                 OpenParen             | CloseParen           |
                 OpenBracket           | CloseBracket         |
                 Keyword Kwrd          | Arrow Arrow          |
                 Word String           | Num Int              |
                 StringLiteral String  |
                 Assign                | Invalid              |
                 BinaryOperation BinOp | UnaryOperation UnOp  |
                 EndOfFile             deriving(Show, Eq);
              
    type Kwrd = Keyword;
    data Keyword = For | In | Case | Let | Start | With deriving(Show, Eq);

    type BinOp = BinaryOperation;
    data BinaryOperation = Union | Difference | CartesianProduct deriving(Show, Eq);

    type UnOp = UnaryOperation;
    data UnaryOperation = Power deriving(Show, Eq);

    data Arrow = L | R deriving(Show, Eq);
}