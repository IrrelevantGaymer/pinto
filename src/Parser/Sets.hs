{-# LANGUAGE TupleSections #-}
module Sets where {
    import Pattern (Pat, getShape, Pattern (..), PatShape, PatternShape (..));
    import Text.Printf (printf);
    
    type UnOp = UnarySetOperation;
    data UnarySetOperation = PowerSet deriving(Show);
    type BinOp = BinarySetOperation;
    data BinarySetOperation = Union | Difference | CartesianProduct deriving(Show);

    type Precedence = Int;
    getPrecedence :: BinOp -> Precedence;
    getPrecedence CartesianProduct = 1;
    getPrecedence Union            = 2;
    getPrecedence Difference       = 2;

    data SetDef = Set ![Pat] |
        Word String |
        UnOpSet !UnOp !SetDef |
        BinOpSet !BinOp !SetDef !SetDef |
        Id !SetDef deriving(Show);

    -- TODO: make this a HashMap instead
    type Sets = [(String, SetDef)];

    inSetByPred :: Sets -> (Pat -> Bool) -> SetDef -> Bool;
    inSetByPred _ f (Set pats) = or $ fmap f pats;
    inSetByPred sets f (Word set) = 
        or $ inSetByPred sets f . snd <$> findByKey fst set sets 
    where {
        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey g k (x:xs)
            | g x == k  = Just x
            | otherwise = findByKey g k xs
    };
    inSetByPred _ _ (UnOpSet PowerSet _) = False;
    inSetByPred sets f (BinOpSet Union setA setB) =
        inSetByPred sets f setA || inSetByPred sets f setB;
    inSetByPred sets f (BinOpSet Difference setA setB) =
        not $ inSetByPred sets f setB && inSetByPred sets f setA;
    inSetByPred _ _ (BinOpSet CartesianProduct _ _) = False;
    inSetByPred _ _ (Id set) = error $ printf
        "Id %s should only be used for parsing set expressions"
        $ show set;

    data SetShape = SetRef SetDef | IdxInSetRef [Int] SetDef deriving (Show);

    -- TODO: make this a HashMap instead
    type Keys = [(String, SetShape)];

    valueInSet :: Sets -> Pat -> SetDef -> Bool;
    valueInSet sets value = inSetByPred sets (value ==);

    valueInIdxSet :: Sets -> [Int] -> String -> SetDef -> Bool;
    valueInIdxSet sets indices value = inSetByPred sets (valueIn indices value) where {
        valueIn :: [Int] -> String -> Pat -> Bool;
        valueIn (idx:idcs) k (Tuple vs) = or $ valueIn idcs k <$> vs !? idx;
        valueIn (idx:idcs) k (List vs) = or $ valueIn idcs k <$> vs !? idx;
        valueIn [] k (Value v) = k == v;
        valueIn _ _ _ = False;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    };

    getPatternShape :: SetDef -> Sets -> Maybe PatShape;
    getPatternShape (Set pats) _ = Just $ mconcat $ fmap getShape pats;
    getPatternShape (UnOpSet PowerSet _) _ = Just V;
    getPatternShape (BinOpSet Union setA setB) sets = do {
        shapeA <- getPatternShape setA sets;
        shapeB <- getPatternShape setB sets;
        return $ shapeA <> shapeB;
    };
    getPatternShape (BinOpSet Difference _ _) _ = undefined;
    getPatternShape (BinOpSet CartesianProduct setA setB) sets = do {
        shapeA <- getPatternShape setA sets;
        shapeB <- getPatternShape setB sets;
        return $ combineShape shapeA shapeB;
    } where {
        combineShape a b
            | (T i patA, T j patB) <- (a, b) = T (i + j) $ patA <> patB
            | T i patA <- a = T (i + 1) $ patA <> b
            | T j patB <- b = T (j + 1) $ patB <> a
            | otherwise = T 2 $ a <> b
    };
    getPatternShape (Word setLookup) sets
        | Just setDef <- set = getPatternShape setDef sets
        | Nothing     <- set = Nothing
    where {
        set = snd <$> findByKey fst setLookup sets;
        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey f k (x:xs)
            | f x == k  = Just x
            | otherwise = findByKey f k xs
    };
    getPatternShape (Id set) _ = error $ printf
        "Id %s should only be used for parsing set expressions"
        $ show set;

    getPatternKeys :: Pat -> SetDef -> Sets -> Maybe Keys;
    getPatternKeys (Value pat) def _ = Just [(pat, SetRef def)];
    getPatternKeys (List _) _ _ = undefined;
    getPatternKeys (Tuple pats) set@(BinOpSet CartesianProduct _ _) sets = do {
        (rest, keys) <- getKey pats set;
        if null rest then
            return keys;
        else
            Nothing;
    } where {
        getKey ps (BinOpSet CartesianProduct setA' setB') = do {
            (ps', keys) <- getKey ps setA';
            (ps'', keys') <- getKey ps' setB';
            return (ps'', keys ++ keys');
        };
        getKey (p:ps) s = (ps,) <$> getPatternKeys p s sets;
        getKey [] _ = Nothing;
    };
    getPatternKeys (Tuple pats) def sets
        | Just (T len defs) <- defShape = if length pats == len then
            getKeys (zip [0..] pats) [] defs;
        else
            Nothing
        | otherwise = Nothing
    where {
        defShape = getPatternShape def sets;
        getKeys ((idx, x):xs) idcs shp
            | Value v <- x, V <- shp = ((v, IdxInSetRef (idcs ++ [idx]) def) :) 
                <$> getKeys xs idcs shp
            | Tuple vs <- x, T l shp' <- shp = if l == length vs then
                getKeys (zip [0..] vs) (idcs ++ [idx]) shp'
            else 
                Nothing
            | List _ <- x, L _ <- shp = undefined
            | otherwise = Nothing;
        getKeys [] _ _ = Just []
    };
    getPatternKeys Discard _ _ = Just [];
}