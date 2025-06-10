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

    data SetDef = Set ![Pat String] |
        Word String |
        UnOpSet !UnOp !SetDef |
        BinOpSet !BinOp !SetDef !SetDef |
        Id !SetDef deriving(Show);

    -- TODO: make this a HashMap instead
    type Sets = [(String, SetDef)];

    inSetByPred :: Sets -> (Pat String -> Bool) -> SetDef -> Bool;
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

    valueInSet :: Sets -> Pat String -> SetDef -> Bool;
    valueInSet sets value = inSetByPred sets (value ==);

    valueInIdxSet :: Sets -> [Int] -> String -> SetDef -> Bool;
    valueInIdxSet sets indices value = inSetByPred sets (valueIn indices value) where {
        valueIn :: [Int] -> String -> Pat String -> Bool;
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
}