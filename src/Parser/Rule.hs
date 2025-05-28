module Rule where {
    import Direction ( Dir );
    import Pattern ( Pat );

    data Rule = Rule {
        ruleCurrentState :: Pat,
        ruleFromValue :: Pat,
        ruleToValue :: Pat,
        ruleDir :: Dir,
        ruleNextState :: Pat
    } deriving(Show);
}