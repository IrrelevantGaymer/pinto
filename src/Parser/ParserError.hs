module ParserError where {
    import Lexer ( Atom );
    import Tokens ( Tkn );

    data ParserError = 
        UnexpectedToken { 
            expectedTkn :: Tkn, 
            received :: Atom Tkn 
        } |
        ExpectedWord {
            received :: Atom Tkn  
        } |
        Empty -- used for empty in Alternative for Parser
    deriving (Show);
}