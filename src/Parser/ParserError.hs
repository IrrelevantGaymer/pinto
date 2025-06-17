module Parser.ParserError where {
    import Lexer.Lexer ( Atom );
    import Lexer.Tokens ( Tkn );

    data ParserError = 
        UnexpectedToken { 
            expected :: Tkn, 
            received :: Atom Tkn 
        } |
        ExpectedWord {
            received :: Atom Tkn  
        } |
        ExpectedString {
            received :: Atom Tkn
        } |
        ExpectedClose {
            openTkn  :: Atom Tkn,
            expected :: Tkn,
            received :: Atom Tkn
        } |
        ExpectedForCase {
            caseTkn    :: Atom Tkn,
            caseExpect :: ExpectForCase,
            received   :: Atom Tkn
        } |
        ExpectedForUQ {
            forTkn   :: Atom Tkn,
            uqExpect :: ExpectForUQ,
            received :: Atom Tkn
        } |
        ExpectedRuleForUQ {
            forTkn   :: Atom Tkn,
            received :: Atom Tkn,
            failure  :: ParserError
        } |
        ExpectedForSetDef {
            letTkn       :: Atom Tkn,
            setDefExpect :: ExpectForSetDef,
            received     :: Atom Tkn
        } |
        ExpectedForSetBuilder {
            startTkn         :: Atom Tkn,
            setBuilderExpect :: ExpectForSetBuilder,
            received         :: Atom Tkn
        } |
        CouldNotParseSet {
            startSet :: Atom Tkn,
            received :: Atom Tkn
        } |
        CouldNotParseRule {
            received :: Atom Tkn
        } |
        CouldNotParsePattern {
            received :: Atom Tkn
        } |
        ExpectedPat {
            startSet :: Atom Tkn,
            received :: Atom Tkn
        } |
        ExpectedNode {
            received :: Atom Tkn
        } |
        ExpectedBinaryOp {
            received :: Atom Tkn
        } |
        ExpectedUnaryOp {
            received :: Atom Tkn
        } |
        CouldNotFindSet {
            received :: Atom Tkn
        } |
        Debug {
            msg      :: String,
            received :: Atom Tkn
        }
    deriving (Show);

    data ExpectForCase = CurrentState | FromValue | ToValue | CaseDir | NextState deriving(Show); 
    data ExpectForUQ = UQPat | UQIn | UQPatSet | Rules deriving(Show);
    data ExpectForSetDef = Name | SetDefAssign deriving(Show);
    data ExpectForSetBuilder = BuildPat | BuildFor | BuildMatch | BuildIn | BuildPatSet deriving(Show);
}