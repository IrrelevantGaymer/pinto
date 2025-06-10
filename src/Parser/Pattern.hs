{-# LANGUAGE FlexibleInstances #-}

module Pattern where {
    type Pat a = Pattern a;
    data Pattern a = Value a
        | List  [Pattern a]
        | Tuple [Pattern a]
        | Discard
        deriving(Eq);

    instance Functor Pattern where {
        fmap f (Value x) = Value $ f x;
        fmap f (List xs) = List $ fmap f <$> xs;
        fmap f (Tuple xs) = Tuple $ fmap f <$> xs;
        fmap _ Discard = Discard;
    };

    instance Show (Pattern String) where {
        show (Value value) = value;
        show (List values) = "[" ++ s values ++ "]" where {
            s  (x:xs) = show x ++ s2 xs;
            s  [] = "";
            s2 (x:xs) = " " ++ show x ++ s2 xs;
            s2 [] = "";
        };
        show (Tuple values) = "(" ++ s values ++ ")" where {
            s  (x:xs) = show x ++ s2 xs;
            s  [] = "";
            s2 (x:xs) = " " ++ show x ++ s2 xs;
            s2 [] = "";
        };
        show Discard = "_"
    };

    type PatShape = PatternShape;
    data PatternShape = V | L PatternShape | T Int PatternShape | D deriving (Show);

    instance Semigroup PatShape where {
        D <> x = x;
        x <> D = x;
        L x <> L y = L $ x <> y;
        T i x <> T j y = T (i + j) $ x <> y; 
        _ <> _ = V;
    };

    instance Monoid PatShape where {
        mempty = D;
    };

    getShape :: Pat a -> PatShape;
    getShape (Tuple pats) = T (length pats) $ mconcat $ fmap getShape pats;
    -- This will require some experimentation
    getShape (List pats) = L $ mconcat $ fmap getShape pats;
    getShape _ = V;
}