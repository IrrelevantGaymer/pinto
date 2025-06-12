module Parser.Rule.UQRule where {
    import Parser.Sets (Sets, Keys, SetShape (..), valueInSet, valueInIdxSet, getPatternKeys);
    import Text.Printf (printf);
    import {-# SOURCE #-} Parser.Rule.Rule (UQRule, UniversalQuantifierRule (..), Rule (..), BasicRule (..));
    import Parser.Pattern (Pattern(..), Pat);
    import Parser.Tape (Tape (..));
    import qualified Parser.Direction as Dir;
    import Data.Maybe (fromMaybe, isJust);
    import Control.Monad (join);
    import Data.Foldable (find);
    
    showUQRule :: UQRule -> Sets -> String;
    showUQRule (UQRule pat patSet rules) sets = printf "UQRule {keys: %s, rules: [%s]}" (show keys) (showRules rules) where {
        keys = getPatternKeys pat patSet sets;
        showRules [SimpleRule basicRule] = show basicRule;
        showRules ((SimpleRule basicRule):rs) = show basicRule ++ ", " ++ showRules rs;
        showRules [ComplexRule uqRule] = showUQRule uqRule sets;
        showRules ((ComplexRule uqRule):rs) = showUQRule uqRule sets ++ ", " ++ showRules rs;
        showRules [] = "";
    };

    type PatKeys = [(String, Pat)];

    applyUQRule :: Tape -> Sets -> UQRule -> Tape;
    applyUQRule tape sets rule 
        | Just tape' <- tryApply keys rRules = tape'
        | otherwise = error "Could not apply UQ Rule"
    where {
        (Tape tName tState tValues tIdx) = tape;
        (UQRule rPat rPatSet rRules) = rule;
        tValue = tValues !! tIdx;
        keys = fromMaybe [] (getPatternKeys rPat rPatSet sets);

        tryApply :: Keys -> [Rule] -> Maybe Tape;
        tryApply ks rs = join $ find isJust $ fmap (apply ks) rs;

        apply :: Keys -> Rule -> Maybe Tape;
        apply ks (SimpleRule basicRule) = do {
            ruleKeys <- join $ combinePatKeys 
                <$> getPatKeys sets ks rCurrState' tState
                <*> getPatKeys sets ks rFromValue' tValue;
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
            (UQRule rPat' rPatSet' rRules') = uqRule;
            ks' = ks ++ fromMaybe [] (getPatternKeys rPat' rPatSet' sets)
        };

        constructNewValue 
            :: PatKeys 
            -> Maybe Pat 
            -> Maybe Pat 
            -> Pat 
            -> Pat;
        constructNewValue rKeys _ _ (Value v) 
            | Just v' <- kLookup = v'
            | Nothing <- kLookup = Value v
        where {
            kLookup = snd <$> findByKey fst v rKeys;
        };
        constructNewValue _ _ _ (Num v) = Num v;
        constructNewValue rKeys (Just (Tuple ts)) (Just (Tuple fs)) (Tuple vs) = 
            Tuple $ constructNewValues rKeys ts fs vs;
        constructNewValue rKeys (Just (Value _)) (Just (Value _)) (Tuple vs) = 
            Tuple $ constructNewValues rKeys [] [] vs;
        --This feels weird
        constructNewValue rKeys (Just (Num _)) (Just (Num _)) (Tuple vs) = 
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
        --This feels weird
        constructNewValue rKeys (Just (Num _)) (Just (Num _)) (List vs) = 
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
            -> [Pat] 
            -> [Pat] 
            -> [Pat] 
            -> [Pat];
        constructNewValues _ _ _ [] = [];
        constructNewValues rKeys ts fs (v:vs) = 
            constructNewValue rKeys (headMaybe ts) (headMaybe fs) v 
            : constructNewValues rKeys ts fs vs
        where {
            headMaybe :: [a] -> Maybe a;
            headMaybe []    = Nothing;
            headMaybe (x:_) = Just x;
        };

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

    canApplyUQRule :: Tape -> Keys -> Sets -> UQRule -> Bool;
    canApplyUQRule tape keys sets rule = or $ fmap canApply rRules where {
        (Tape _ tState tValues tIdx) = tape;
        (UQRule rPat rPatSet rRules) = rule;
        tValue = tValues !! tIdx;
        keys' = keys ++ fromMaybe [] (getPatternKeys rPat rPatSet sets);

        canApply :: Rule -> Bool;
        canApply (SimpleRule basicRule) = or $ matchPatKeys 
            <$> stateKeys
            <*> valueKeys
        where {
            (BasicRule rCurrState' rFromValue' _ _ _) = basicRule;
            stateKeys = getPatKeys sets keys' rCurrState' tState;
            valueKeys = getPatKeys sets keys' rFromValue' tValue;
        };
        canApply (ComplexRule uqRule) = canApplyUQRule tape keys' sets uqRule;

        matchPatKeys :: PatKeys -> PatKeys -> Bool;
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

    getPatKeys :: Sets -> Keys -> Pat -> Pat -> Maybe PatKeys;
    getPatKeys sets ks r@(Value key) t 
        | r == t                     = Just []
        | or (inShape t <$> uqShape) = Just [(key, t)]
        | otherwise = Nothing 
    where {
        uqShape = snd <$> findByKey fst key ks;
        inShape v (SetRef shp) = valueInSet sets v shp;
        inShape (Value v) (IdxInSetRef i shp) = valueInIdxSet sets i v shp;
        --Maybe rewrite this.  I kinda hate converting the integer to a string
        inShape (Num v) (IdxInSetRef i shp) = valueInIdxSet sets i (show v) shp; 
        inShape _ _ = False;

        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey f k (x:xs)
            | f x == k  = Just x
            | otherwise = findByKey f k xs;
    };
    getPatKeys _ _ (Num k) (Num t) = if k == t then Just [] else Nothing;
    getPatKeys sets ks (Tuple uqPats) (Tuple tPats)
        | length uqPats == length tPats = 
            foldl combine (Just []) $ uncurry (getPatKeys sets ks) <$> zip uqPats tPats
        | otherwise = Nothing
    where {
        combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
        combine a b = (++) <$> a <*> b;
    };
    getPatKeys sets ks (List uqPats) (List tPats)
        | length uqPats == length tPats = 
            foldl combine (Just []) $ uncurry (getPatKeys sets ks) <$> zip uqPats tPats
        | otherwise = Nothing
    where {
        combine :: Maybe [a] -> Maybe [a] -> Maybe [a];
        combine a b = (++) <$> a <*> b;
    };
    getPatKeys _ _ Discard _ = Just [];
    getPatKeys _ _ _ _ = Nothing;
}
