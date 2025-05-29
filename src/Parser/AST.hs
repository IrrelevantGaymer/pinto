module AST where {
    import Rule (Rule);
    import Pattern (Pat);
    import Tape (Tape);
    import Sets (SetDef);
    
    data AST = AST {
        astStartingTapes :: [Tape],
        astRules :: [Rule]
    } deriving(Show);

    emptyAST :: AST;
    emptyAST = AST [] [] [];

    data Node = RuleNode Rule | TapeNode Tape;

    data Map = Map {
        mapName :: String,
        mapFromSet :: SetDef,
        mapToSet :: SetDef,
        mapMappings :: [Mapping]
    } deriving(Show);

    data Mapping = Mapping {
        mappingFromPattern :: Pat,
        mappingToPattern :: Pat
    } deriving(Show);
}