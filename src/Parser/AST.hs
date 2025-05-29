module AST where {
    import Rule (Rule);
    import Pattern (Pat);
    import Tape (Tape);
    import Sets (SetDef);
    
    data AST = AST {
        astSets :: [(String, SetDef)],
        astStartingTapes :: [Tape],
        astRules :: [Rule]
    } deriving(Show);

    emptyAST :: AST;
    emptyAST = AST [] [] [];

    data Node = SetNode (String, SetDef) | RuleNode Rule | TapeNode Tape deriving(Show);

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