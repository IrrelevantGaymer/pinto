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
}