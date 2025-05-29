module Rule where {
    import Direction ( Dir );
    import Pattern ( Pat, Pattern (..) );
    import Tape (Tape (..));
    import qualified Direction as Dir;
    
    data Rule = Rule {
        ruleCurrentState :: Pat,
        ruleFromValue :: Pat,
        ruleToValue :: Pat,
        ruleDir :: Dir,
        ruleNextState :: Pat
    } deriving(Show);

    applyRule :: Tape -> Rule -> Tape;
    applyRule tape rule = Tape tName rNextState newTValues newTIdx where {
        (Tape tName _ tValues tIdx) = tape;
        (Rule _ rFromValue rToValue rDir rNextState) = rule;
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

    canApplyRule :: Tape -> Rule -> Bool;
    canApplyRule tape rule = case tapeValueAtIdx of {
        Just tValue -> tState == rCurrentState && tValue == rFromValue;
        Nothing -> False;
    } where {
        (Tape _ tState tValues tIdx) = tape;
        (Rule rCurrentState rFromValue _ _ _) = rule;
        tapeValueAtIdx = tValues !? tIdx;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    }
}