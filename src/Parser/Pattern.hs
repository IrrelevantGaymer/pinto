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
}