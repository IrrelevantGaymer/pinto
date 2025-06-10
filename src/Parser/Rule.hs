{-# LANGUAGE TypeSynonymInstances #-}
module Rule where {
    import Direction ( Dir );
    import Pattern ( Pat, Pattern (..) );
    import Tape (Tape (..));
    import qualified Direction as Dir;
    import Sets (SetDef (..), getPatternKeys, SetShape (..), valueInSet, valueInIdxSet, Sets, Keys);
    import Text.Printf (printf);
    
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
        uqRules  :: [Rule]
    };

    showUQRule :: UQRule -> Sets -> String;
    showUQRule (UniversalQuantifierRule pat patSet rules) sets = printf "UQRule {keys: %s, rules: %s}" (show keys) (showRules rules) where {
        keys = getPatternKeys pat patSet sets;
        showRules [SimpleRule basicRule] = show basicRule;
        showRules ((SimpleRule basicRule):rs) = show basicRule ++ ", " ++ showRules rs;
        showRules [ComplexRule uqRule] = showUQRule uqRule sets;
        showRules ((ComplexRule uqRule):rs) = showUQRule uqRule sets ++ ", " ++ showRules rs;
        showRules [] = "";
    };

    {-
     - TODO: maybe have applyRule return a Maybe Tape or a (Bool, Tape)
     - so that we don't have this two-step process of canApplyRule -> applyRule
     - If canApplyRule/applyRule are expensive operations, then this
     - could have 2x improvements. 
     -}

    applyRule :: Tape -> Sets -> Rule -> Tape;
    applyRule tape _ (SimpleRule basicRule) = applyBasicRule tape basicRule;
    applyRule tape sets (ComplexRule uqRule) = applyUQRule tape sets uqRule;

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

    type PatKeys = [(String, Pat String)];

    applyUQRule :: Tape -> Sets -> UQRule -> Tape;
    applyUQRule tape sets rule 
        | Just tape' <- tryApply keys rRules = tape'
        | otherwise = error "Could not apply UQ Rule"
    where {
        (Tape tName tState tValues tIdx) = tape;
        (UniversalQuantifierRule rPat rPatSet rRules) = rule;
        tValue = tValues !! tIdx;
        keys = fromMaybe [] (getPatternKeys rPat rPatSet sets);

        tryApply :: Keys -> [Rule] -> Maybe Tape;
        tryApply ks rs = join $ find isJust $ fmap (apply ks) rs;

        apply :: Keys -> Rule -> Maybe Tape;
        apply ks (SimpleRule basicRule) = do {
            ruleKeys <- join $ combinePatKeys 
                <$> getPatKeys ks rCurrState' tState
                <*> getPatKeys ks rFromValue' tValue;
            let {
                tValue' = constructNewValue ruleKeys 
                    (Just tValue) (Just rFromValue') rToValue';
                tState' = constructNewValue ruleKeys 
                    (Just tState) (Just rCurrState') rNextState';
                tValues' = take tIdx tValues ++ tValue' : drop (tIdx + 1) tValues;
                tIdx' = case rDir' of {
                    Dir.L -> tIdx - 1;
                    Dir.R -> tIdx + 1;
                };
            };
            return $ Tape tName tState' tValues' tIdx';
        } where {
            (BasicRule rCurrState' rFromValue' rToValue' rDir' rNextState') = basicRule;
        };
        apply ks (ComplexRule uqRule) = tryApply ks' rRules' where {
            (UniversalQuantifierRule rPat' rPatSet' rRules') = uqRule;
            ks' = ks ++ fromMaybe [] (getPatternKeys rPat' rPatSet' sets)
        };

        constructNewValue 
            :: PatKeys 
            -> Maybe (Pat String) 
            -> Maybe (Pat String) 
            -> Pat String 
            -> Pat String;
        constructNewValue rKeys _ _ (Value v) 
            | Just v' <- kLookup = v'
            | Nothing <- kLookup = Value v
        where {
            kLookup = snd <$> findByKey fst v rKeys;
        };
        constructNewValue rKeys (Just (Tuple ts)) (Just (Tuple fs)) (Tuple vs) = 
            Tuple $ constructNewValues rKeys ts fs vs;
        constructNewValue rKeys (Just (Value _)) (Just (Value _)) (Tuple vs) = 
            Tuple $ constructNewValues rKeys [] [] vs;
        constructNewValue rKeys Nothing Nothing (Tuple vs) = 
            Tuple $ constructNewValues rKeys [] [] vs;
        constructNewValue rKeys t f (Tuple vs) = 
            error (
                "Unmatched tape value and rule from value:" ++ 
                "\n    rule keys: " ++ show rKeys ++
                "\n    from tape: " ++ show t     ++
                "\n    from rule: " ++ show f     ++
                "\n    constructing from: (Tuple " ++ show vs ++ ")"
            );
        constructNewValue rKeys (Just (List ts)) (Just (List fs)) (List vs) = 
            List $ constructNewValues rKeys ts fs vs;
        constructNewValue rKeys (Just (Value _)) (Just (Value _)) (List vs) = 
            List $ constructNewValues rKeys [] [] vs;
        constructNewValue rKeys Nothing Nothing (List vs) = 
            List $ constructNewValues rKeys [] [] vs;
        constructNewValue _ _ _ (List _) = 
            error "Unmatched tape value and rule from value";
        constructNewValue _ (Just t) (Just Discard) Discard = t;
        constructNewValue _ Nothing (Just Discard) Discard = 
            error "Unmatched tape value and rule from value";
        constructNewValue _ _ (Just from) Discard = from;
        constructNewValue _ _ Nothing Discard = 
            error "Invalid use of Discard in rule to value";

        constructNewValues 
            :: PatKeys 
            -> [Pat String] 
            -> [Pat String] 
            -> [Pat String] 
            -> [Pat String];
        constructNewValues _ _ _ [] = [];
        constructNewValues rKeys ts fs (v:vs) = 
            constructNewValue rKeys (headMaybe ts) (headMaybe fs) v 
            : constructNewValues rKeys ts fs vs
        where {
            headMaybe :: [a] -> Maybe a;
            headMaybe []    = Nothing;
            headMaybe (x:_) = Just x;
        };

        getPatKeys :: Keys -> Pat String -> Pat String -> Maybe PatKeys;
        getPatKeys ks r@(Value key) t 
            | r == t                     = Just []
            | or (inShape t <$> uqShape) = Just [(key, t)]
            | otherwise = Nothing 
        where {
            uqShape = snd <$> findByKey fst key ks;
            inShape v (SetRef shp) = valueInSet sets v shp;
            inShape (Value v) (IdxInSetRef i shp) = valueInIdxSet sets i v shp;
            inShape _ _ = False;
        };
        getPatKeys ks (Tuple uqPats) (Tuple tPats)
            | length uqPats == length tPats = 
                foldl combine (Just []) $ uncurry (getPatKeys ks) <$> zip uqPats tPats
            | otherwise = Nothing
        where {
            combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
            combine a b = (++) <$> a <*> b;
        };
        getPatKeys ks (List uqPats) (List tPats)
            | length uqPats == length tPats = 
                foldl combine (Just []) $ uncurry (getPatKeys ks) <$> zip uqPats tPats
            | otherwise = Nothing
        where {
            combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
            combine a b = (++) <$> a <*> b;
        };
        getPatKeys _ Discard _ = Just [];
        getPatKeys _ _ _ = Nothing;

        combinePatKeys :: PatKeys -> PatKeys -> Maybe PatKeys;
        combinePatKeys (a:as) bs 
            | Just (_, bPat) <- b, aPat == bPat = combinePatKeys as bs
            | Just _         <- b               = Nothing
            | Nothing        <- b               = combinePatKeys as (a:bs)
        where {
            (aKey, aPat) = a;
            b = findByKey fst aKey bs;
        };
        combinePatKeys [] bs = Just bs;

        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey f k (x:xs)
            | f x == k  = Just x
            | otherwise = findByKey f k xs;
    };

    canApplyRule :: Tape -> Sets -> Rule -> Bool;
    canApplyRule tape _ (SimpleRule basicRule) = canApplyBasicRule tape basicRule;
    canApplyRule tape sets (ComplexRule uqRule) = canApplyUQRule tape [] sets uqRule;

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

    canApplyUQRule :: Tape -> Keys -> Sets -> UQRule -> Bool;
    canApplyUQRule tape keys sets rule = or $ fmap canApply rRules where {
        (Tape _ tState tValues tIdx) = tape;
        (UniversalQuantifierRule rPat rPatSet rRules) = rule;
        tValue = tValues !! tIdx;
        keys' = keys ++ fromMaybe [] (getPatternKeys rPat rPatSet sets);

        canApply :: Rule -> Bool;
        canApply (SimpleRule basicRule) = or $ matchPatKeys 
            <$> stateKeys
            <*> valueKeys
        where {
            (BasicRule rCurrState' rFromValue' _ _ _) = basicRule;
            stateKeys = getPatKeys rCurrState' tState;
            valueKeys = getPatKeys rFromValue' tValue;
        };
        canApply (ComplexRule uqRule) = canApplyUQRule tape keys' sets uqRule;

        getPatKeys :: Pat String -> Pat String -> Maybe [(String, Pat String)];
        getPatKeys r@(Value key) t 
            | r == t                     = Just []
            | or (inShape t <$> uqShape) = Just [(key, t)]
            --- | otherwise = trace ("wtf " ++ show keys' ++ " " ++ show key ++ " " ++ show (findByKey fst key keys')) Nothing 
            | otherwise = Nothing
        where {
            uqShape = snd <$> findByKey fst key keys';
            inShape v (SetRef shp) = valueInSet sets v shp;
            inShape (Value v) (IdxInSetRef i shp) = valueInIdxSet sets i v shp;
            inShape _ _ = False;

            findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
            findByKey _ _ [] = Nothing;
            findByKey f k (x:xs)
                | f x == k  = Just x
                | otherwise = findByKey f k xs;
        };
        getPatKeys (Tuple uqPats) (Tuple tPats)
            | length uqPats == length tPats =
                foldl combine (Just []) $ uncurry getPatKeys <$> zip uqPats tPats
            | otherwise = Nothing
        where {
            combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
            combine a b = (++) <$> a <*> b;
    };
        getPatKeys (List uqPats) (List tPats)
            | length uqPats == length tPats = 
                foldl combine (Just []) $ uncurry getPatKeys <$> zip uqPats tPats
            | otherwise = Nothing
        where {
            combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
            combine a b = (++) <$> a <*> b;
        };
        getPatKeys Discard _ = Just [];
        getPatKeys _ _ = Nothing;

        matchPatKeys :: [(String, Pat String)] -> [(String, Pat String)] -> Bool;
        matchPatKeys (a:as) bs 
            | Just bPat <- b = (aPat == bPat) && matchPatKeys as bs
            | Nothing   <- b = matchPatKeys as bs
        where {
            (aKey, aPat) = a;
            b = snd <$> findByKey fst aKey bs;

            findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
            findByKey _ _ [] = Nothing;
            findByKey f k (x:xs)
                | f x == k  = Just x
                | otherwise = findByKey f k xs;
        };
        matchPatKeys [] _ = True;
    };
}