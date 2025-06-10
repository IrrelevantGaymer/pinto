module Sets where {
    import Pattern (Pat);
    
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

    -- TODO: make this a HashMap instead
    type Keys = [(String, SetShape)];


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