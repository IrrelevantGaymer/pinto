module AST where {
    import Rule (Rule (..), showUQRule);
    import Pattern (Pat);
    import Tape (Tape);
    import Sets (SetDef, Sets);
    import Text.Printf (printf);
    
    data AST = AST {
        astSets  :: Sets,
        astTapes :: [Tape],
        astRules :: [Rule]
    };

    instance Show AST where {
        show (AST sets tapes rules) = printf "{sets: %s, tapes: %s, rules: [%s]}" (show sets) (show tapes) (showRules rules) where {
            showRules [SimpleRule basicRule] = show basicRule;
            showRules ((SimpleRule basicRule):rs) = show basicRule ++ ", " ++ showRules rs;
            showRules [ComplexRule uqRule] = showUQRule uqRule sets;
            showRules ((ComplexRule uqRule):rs) = showUQRule uqRule sets ++ ", " ++ showRules rs;
            showRules [] = "";

        };
    };

    instance Semigroup AST where {
        (AST aSets aRules aTapes) <> (AST bSets bRules bTapes) = AST 
            (aSets  <> bSets ) 
            (aRules <> bRules) 
            (aTapes <> bTapes);
    };

    instance Monoid AST where {
      mempty = AST [] [] [];
    };

    data Node = SetNode (String, SetDef) | 
        RuleNode Rule | 
        TapeNode Tape;

    data Map = Map {
        mapName     :: String,
        mapFromSet  :: SetDef,
        mapToSet    :: SetDef,
        mapMappings :: [Mapping]
    } deriving(Show);

    data Mapping = Mapping {
        mappingFromPattern :: Pat String,
        mappingToPattern   :: Pat String
    } deriving(Show);
}