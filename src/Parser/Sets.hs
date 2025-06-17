{-# LANGUAGE TupleSections #-}
module Parser.Sets where {
    import Parser.Pattern (Pat, getShape, Pattern (..), PatShape, PatternShape (..));
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
        SetBuilder {
            buildPat    :: !Pat,
            buildMatch  :: !Pat,
            buildSetDef :: !SetDef
        } | 
        RecordSet {
            recordName :: !Pat,
            recordFields :: ![(Pat, SetDef)]
        } |
        Word !String |
        UnOpSet !UnOp !SetDef |
        BinOpSet !BinOp !SetDef !SetDef |
        Id !SetDef deriving(Show);

    data BuiltInSet = All | Int;

    getBuiltInSet :: String -> Maybe BuiltInSet;
    getBuiltInSet "All" = Just All;
    getBuiltInSet "Int" = Just Int;
    getBuiltInSet _     = Nothing;

    -- TODO: make this a HashMap instead
    type Sets = [(String, SetDef)];

    data SetShape = SetRef SetDef | IdxInSetRef [Int] SetDef deriving (Show);

    -- TODO: make this a HashMap instead
    type Keys = [(String, SetShape)];

    valueInSet :: Sets -> Pat -> SetDef -> Bool;
    valueInSet _ value (Set pats) = value `elem` pats;
    valueInSet sets value (Word set)
        | Just All <- tryBuiltIn = True
        | Just Int <- tryBuiltIn, Num _ <- value = True
        | Just Int <- tryBuiltIn = False
        | Nothing  <- tryBuiltIn = or $ valueInSet sets value . snd <$> findByKey fst set sets
    where {
        tryBuiltIn = getBuiltInSet set;

        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey g k (x:xs)
            | g x == k  = Just x
            | otherwise = findByKey g k xs
    };
    valueInSet sets value (SetBuilder pat match set) = or $ do {
        keys <- bKeys;
        return $ matchPat sets keys pat value;
    } where {
        bKeys = getPatternKeys sets match set;

        matchPat :: Sets -> Keys -> Pat -> Pat -> Bool;
        matchPat ss ks m@(Value key) t = m == t || or (inShape t <$> mShape) where {
            mShape = snd <$> findByKey fst key ks;
            inShape v (SetRef shp) = valueInSet ss v shp;
            inShape (Value v) (IdxInSetRef i shp) = valueInIdxSet ss i v shp;
            --Maybe rewrite this.  I kinda hate converting the integer to a string
            inShape (Num v) (IdxInSetRef i shp) = valueInIdxSet ss i (show v) shp; 
            inShape _ _ = False;

            findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
            findByKey _ _ [] = Nothing;
            findByKey f k (x:xs)
                | f x == k  = Just x
                | otherwise = findByKey f k xs;
        };
        matchPat _ _ (Num k) (Num t) = k == t;
        matchPat ss ks (Tuple mPats) (Tuple tPats)
            | length mPats == length tPats = 
                and $ uncurry (matchPat ss ks) <$> zip mPats tPats
            | otherwise = False;
        matchPat ss ks (List mPats) (List tPats)
            | length mPats == length tPats = 
                and $ uncurry (matchPat ss ks) <$> zip mPats tPats
            | otherwise = False;
        matchPat _ _ Discard _ = True;
        matchPat _ _ _ _ = False;
    };
    valueInSet sets (Record vName vFields) (RecordSet sName sFields)
        | vName == sName,
          map fst vFields == map fst sFields
        = and $ zipWith (valueInSet sets) (map snd vFields) (map snd sFields);
    valueInSet _ (Record _ _) _ = False;
    valueInSet _ _ (RecordSet _ _) = False;
    valueInSet _ _ (UnOpSet PowerSet _) = undefined;
    valueInSet sets value (BinOpSet Union setA setB) =
        valueInSet sets value setA || valueInSet sets value setB;
    valueInSet sets value (BinOpSet Difference setA setB) =
        not $ valueInSet sets value setB && valueInSet sets value setA;
    valueInSet sets (Tuple values) set@(BinOpSet CartesianProduct _ _) =
        or $ null <$> matchValuesWithSet values set
    where {
        matchValuesWithSet vs (BinOpSet CartesianProduct setA setB) = do {
            vs' <- matchValuesWithSet vs setA;
            matchValuesWithSet vs' setB;
        };
        matchValuesWithSet (v:vs) s
            | valueInSet sets v s = Just vs
            | otherwise           = Nothing;
        matchValuesWithSet [] _   = Nothing;
    };
    valueInSet _ _ (BinOpSet CartesianProduct _ _) = False;
    valueInSet _ _ (Id set) = error $ printf
        "Id %s should only be used for parsing set expressions"
        $ show set;

    valueInIdxSet :: Sets -> [Int] -> String -> SetDef -> Bool;
    valueInIdxSet _ idcs value (Set pats) = or $ fmap (valueIn idcs value) pats  where {
        valueIn :: [Int] -> String -> Pat -> Bool;
        valueIn (i:is) k (Tuple vs) = or $ valueIn is k <$> vs !? i;
        valueIn (i:is) k (List vs) = or $ valueIn is k <$> vs !? i;
        valueIn (i:is) k (Record _ vs) = or $ valueIn is k <$> map snd vs !? i;
        valueIn [] k (Value v) = k == v;
        valueIn [] k (Num v) = k == show v;
        valueIn _ _ _ = False;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    }; 
    valueInIdxSet sets idcs value (Word set)
        | Just All <- tryBuiltIn = False -- The general pattern shape of All is Value not Tuple
        | Just Int <- tryBuiltIn = False -- The general pattern shape of Int is Value not Tuple
        | Nothing  <- tryBuiltIn = or $ valueInIdxSet sets idcs value . snd <$> findByKey fst set sets
    where {
        tryBuiltIn = getBuiltInSet set;

        findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
        findByKey _ _ [] = Nothing;
        findByKey g k (x:xs)
            | g x == k  = Just x
            | otherwise = findByKey g k xs
    };
    valueInIdxSet sets idcs value (SetBuilder pat match set) = or $ do {
        keys <- bKeys;
        return $ valueIn sets keys idcs value pat;
    } where {
        bKeys = getPatternKeys sets match set;

        valueIn :: Sets -> Keys -> [Int] -> String -> Pat -> Bool;
        valueIn ss ks (i:is) k (Tuple vs) = or $ valueIn ss ks is k <$> vs !? i;
        valueIn ss ks (i:is) k (List vs) = or $ valueIn ss ks is k <$> vs !? i;
        valueIn ss ks (i:is) k (Record _ vs) = or $ valueIn ss ks is k <$> map snd vs !? i;
        valueIn ss ks [] key (Value t) = key == t || or (inShape t <$> mShape) where {
            mShape = snd <$> findByKey fst key ks;
            inShape v (SetRef shp) = valueInSet ss (Value v) shp;
            inShape v (IdxInSetRef i shp) = valueInIdxSet ss i v shp;

            findByKey :: Eq b => (a -> b) -> b -> [a] -> Maybe a;
            findByKey _ _ [] = Nothing;
            findByKey f k (x:xs)
                | f x == k  = Just x
                | otherwise = findByKey f k xs;
        };
        valueIn _ _ [] key (Num t) = key == show t;
        valueIn _ _ _ _ _ = undefined;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    };
    valueInIdxSet sets (idx:idcs) value (RecordSet _ fieldSets) =
        or $ valueInIdxSet sets idcs value <$> fmap snd (fieldSets !? idx)
    where {
        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? i
            | i > 0     = xs !? (i - 1)
            | otherwise = Nothing;
    };
    valueInIdxSet _ [] _ (RecordSet _ _) = False;
    valueInIdxSet _ _ _ (UnOpSet PowerSet _) = undefined;
    valueInIdxSet sets idcs value (BinOpSet Union setA setB) =
        valueInIdxSet sets idcs value setA || valueInIdxSet sets idcs value setB;
    valueInIdxSet sets idcs value (BinOpSet Difference setA setB) =
        not $ valueInIdxSet sets idcs value setB && valueInIdxSet sets idcs value setA;
    -- As far as I know, trying to check if an indexed value is in a
    -- cartesian product set is useless, however it would be relatively easy
    -- to implement.  There might be some crazy stuff with power sets later.
    valueInIdxSet _ _ _ (BinOpSet CartesianProduct _ _) = undefined;
    valueInIdxSet _ _ _ (Id set) = error $ printf
        "Id %s should only be used for parsing set expressions"
        $ show set;

    getPatternShape :: SetDef -> Sets -> Maybe PatShape;
    getPatternShape (Set pats) _ = Just $ mconcat $ fmap getShape pats;
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
    -- TODO: add checking if the set builder is properly formatted
    getPatternShape (SetBuilder pat _ _) _ = Just $ getShape pat;
    getPatternShape (RecordSet name fields) sets = R name <$>
        traverse sequenceA (second (`getPatternShape` sets) <$> fields);
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
    getPatternShape (Id set) _ = error $ printf
        "Id %s should only be used for parsing set expressions"
        $ show set;

    getPatternKeys :: Sets -> Pat -> SetDef -> Maybe Keys;
    getPatternKeys _ (Value pat) def = Just [(pat, SetRef def)];
    getPatternKeys _ (Num _) _ = error "Cannot use an integer for Universal Qualification identifiers";
    getPatternKeys _ (List _) _ = undefined;
    getPatternKeys sets (Tuple pats) set@(BinOpSet CartesianProduct _ _) = do {
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
        getKey (p:ps) s = (ps,) <$> getPatternKeys sets p s;
        getKey [] _ = Nothing;
    };
    getPatternKeys sets (Record vName vs) def
        | Just (R sName shps) <- defShape, 
          vName == sName, 
          length vs == length shps,
          map fst vs == map fst shps
        = concat <$> zipWithM (getIdxKey [] def) (map snd shps) (zip [0..] (map snd vs))
        | otherwise = Nothing
    where {
        defShape = getPatternShape def sets;
    getIdxKey :: [Int] -> SetDef -> PatShape -> (Int, Pat) -> Maybe Keys;
    getIdxKey idcs def shp (idx, x)
        | Value v <- x, V <- shp = Just [(v, IdxInSetRef (idcs ++ [idx]) def)]
        | Tuple vs <- x, 
          T shps <- shp, 
          length vs == length shps
        = concat <$> zipWithM (getIdxKey idcs def) shps (zip [0..] vs)
            | List _ <- x, L _ <- shp = undefined
        | Record vName vs <- x, 
          R sName shps <- shp, 
          vName == sName, 
          length vs == length shps,
          map fst vs == map fst shps
        = concat <$> zipWithM (getIdxKey idcs def) (map snd shps) (zip [0..] (map snd vs))
        | otherwise = Nothing;
}