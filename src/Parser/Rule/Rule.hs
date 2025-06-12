module Parser.Rule.Rule where {
    import Parser.Pattern (Pat);
    import Parser.Direction (Dir);
    import Parser.Sets (SetDef, Sets);
    import Parser.Tape (Tape);
    import Parser.Rule.BasicRule (applyBasicRule, canApplyBasicRule);
    import Parser.Rule.UQRule (applyUQRule, canApplyUQRule);
    
    data Rule = SimpleRule BasicRule | ComplexRule UQRule;

    data BasicRule = BasicRule {
        ruleCurrentState :: Pat,
        ruleFromValue    :: Pat,
        ruleToValue      :: Pat,
        ruleDir          :: Dir,
        ruleNextState    :: Pat
    } deriving(Show);

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

    {-
     - TODO: maybe have applyRule return a Maybe Tape or a (Bool, Tape)
     - so that we don't have this two-step process of canApplyRule -> applyRule
     - If canApplyRule/applyRule are expensive operations, then this
     - could have 2x improvements. 
     -}
    applyRule :: Tape -> Sets -> Rule -> Tape;
    applyRule tape _ (SimpleRule basicRule) = applyBasicRule tape basicRule;
    applyRule tape sets (ComplexRule uqRule) = applyUQRule tape sets uqRule;

    canApplyRule :: Tape -> Sets -> Rule -> Bool;
    canApplyRule tape _ (SimpleRule basicRule) = canApplyBasicRule tape basicRule;
    canApplyRule tape sets (ComplexRule uqRule) = canApplyUQRule tape [] sets uqRule;
}
