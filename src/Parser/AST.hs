module AST where {
    import Rule (Rule);
    import Pattern (Pat);
    import Tape (Tape);
    import Sets (SetDef);
    
    data AST = AST {
        astSets :: [(String, SetDef)],
        astTapes :: [Tape],
        astRules :: [Rule]
    } deriving(Show);

    instance Semigroup AST where {
        (AST aSets aRules aTapes) <> (AST bSets bRules bTapes) = AST 
            (aSets  <> bSets ) 
            (aRules <> bRules) 
            (aTapes <> bTapes);
    };

    instance Monoid AST where {
      mempty = AST [] [] [];
    };

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