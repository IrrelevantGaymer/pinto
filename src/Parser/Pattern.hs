module Pattern where {
    type Pat = Pattern;
    data Pattern = Value String | List [Pattern] | Tuple [Pattern] | Discard deriving(Eq);

    instance Show Pattern where {
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