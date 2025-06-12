module Parser.Rule.Rule where {
    import Parser.Pattern (Pat);
    import Parser.Direction (Dir);
    import Parser.Sets (SetDef);

    data Rule = SimpleRule BasicRule | ComplexRule UQRule;

    data BasicRule = BasicRule {
        ruleCurrentState :: Pat,
        ruleFromValue    :: Pat,
        ruleToValue      :: Pat,
        ruleDir          :: Dir,
        ruleNextState    :: Pat
    };

    instance Show BasicRule where {
        
    };

    -- TODO: allow set definitions to be bound by rule keys
    -- example: for (state a) in All * All for b in All - {a} case (Eq a b) state False -> (Reset state)
    type UQRule = UniversalQuantifierRule;
    data UniversalQuantifierRule = UQRule {
        uqPat    :: Pat,
        uqPatSet :: SetDef,
        uqRules  :: [Rule]
    };

    {-
     - Print = for (a b) in All * All case (Print a) b b -> (Reset a)
     - Read  = for (a b c) in All * All * All case (Read  a) b c -> (Reset a)
     -       note: Read is weird/special because it gets the new tape value
     -             from the outside world
     - GetHeadAddr = for (a b c) in All * All * Int case (GetHeadAddr a) b c -> (Reset a) 
     -}
    data BuiltInRule = Print | Read | GetHeadAddr;
}