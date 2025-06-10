module Rule where {
    import Direction ( Dir );
    import Pattern ( Pat, Pattern (..) );
    import Tape (Tape (..));
    import qualified Direction as Dir;
    import Sets (SetDef);
    
    data Rule = SimpleRule BasicRule | ComplexRule UQRule deriving(Show);

    data BasicRule = BasicRule {
        ruleCurrentState :: Pat String,
        ruleFromValue    :: Pat String,
        ruleToValue      :: Pat String,
        ruleDir          :: Dir,
        ruleNextState    :: Pat String
    } deriving(Show);

    type UQRule = UniversalQuantifierRule;
    data UniversalQuantifierRule = UniversalQuantifierRule {
        uqPat    :: Pat String,
        uqPatSet :: SetDef,
        uqRules :: [Rule]
    } deriving(Show);

    applyRule :: Tape -> Rule -> Tape;
    applyRule tape (SimpleRule basicRule) = applyBasicRule tape basicRule;
    applyRule tape (ComplexRule uqRule) = applyUQRule tape uqRule;

    applyBasicRule :: Tape -> BasicRule -> Tape;
    applyBasicRule tape rule = Tape tName rNextState newTValues newTIdx where {
        (Tape tName _ tValues tIdx) = tape;
        (BasicRule _ rFromValue rToValue rDir rNextState) = rule;
        newTValues = case (rFromValue, rToValue) of {
            (Discard, Discard) -> tValues;
            (_, Discard) -> error "Cannot apply basic rule with defined from Value and Discarded to Value";
            (_, to) -> take tIdx tValues ++ to : drop (tIdx + 1) tValues;
        };
        newTIdx = case rDir of {
            Dir.L -> tIdx - 1;
            Dir.R -> tIdx + 1;
        };
    };

    applyUQRule :: Tape -> UQRule -> Tape;
    applyUQRule = undefined;

    canApplyRule :: Tape -> Rule -> Bool;
    canApplyRule tape (SimpleRule basicRule) = canApplyBasicRule tape basicRule;
    canApplyRule tape (ComplexRule uqRule) = canApplyUQRule tape uqRule;

    canApplyBasicRule :: Tape -> BasicRule -> Bool;
    canApplyBasicRule tape rule = case tapeValueAtIdx of {
        Just tValue -> tState == rCurrentState && tValue == rFromValue;
        Nothing -> False;
    } where {
        (Tape _ tState tValues tIdx) = tape;
        (BasicRule rCurrentState rFromValue _ _ _) = rule;
        tapeValueAtIdx = tValues !? tIdx;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    };

    canApplyUQRule :: Tape -> UQRule -> Bool;
    canApplyUQRule = undefined;
}