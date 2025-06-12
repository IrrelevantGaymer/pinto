module Parser.Rule.BasicRule where {
    import {-# SOURCE #-} Parser.Rule.Rule;
    import Parser.Tape (Tape(..));
    import Parser.Pattern (Pattern(..));
    import qualified Parser.Direction as Dir;
    
    
    -- TODO: add support for nested Discards
    applyBasicRule :: Tape -> BasicRule -> Tape;
    applyBasicRule tape rule = Tape tName rNextState newTValues newTIdx where {
        (Tape tName _ tValues tIdx) = tape;
        (BasicRule _ rFromValue rToValue rDir rNextState) = rule;
        newTValues = case (rFromValue, rToValue) of {
            (Discard, Discard) -> tValues;
            (from, Discard) -> take tIdx tValues ++ from : drop (tIdx + 1) tValues;
            (_, to) -> take tIdx tValues ++ to : drop (tIdx + 1) tValues;
        };
        newTIdx = case rDir of {
            Dir.L -> tIdx - 1;
            Dir.R -> tIdx + 1;
        };
    };

    canApplyBasicRule :: Tape -> BasicRule -> Bool;
    canApplyBasicRule tape rule = 
        matchPat rCurrState tState && 
        matchPat rFromValue tValue 
    where {
        (Tape _ tState tValues tIdx) = tape;
        (BasicRule rCurrState rFromValue _ _ _) = rule;
        tValue = tValues !! tIdx;
        matchPat Discard _ = True;
        matchPat r t = r == t;
    };
}
