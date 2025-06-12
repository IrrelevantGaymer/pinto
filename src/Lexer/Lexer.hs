module Lexer where {
    import Control.Applicative ( Alternative((<|>), empty) );
    import Data.Char ( isSpace, isDigit, ord, toLower );

    import Tokens (Token(..), Keyword(..), Tkn, Arrow (..), BinaryOperation (..), UnaryOperation (Power));
    import Data.Foldable (asum);
    import Data.Bits (Bits(shiftL));
    import Text.Read (readMaybe);
    
    type FileName = String;
    type Row = Int;
    type Col = Int;

    data Atom a = Atom {
        atomValue    :: a,
        atomFileName :: FileName,
        atomPos      :: (Row, Col)
    };

    instance Functor Atom where {
        fmap f (Atom x fileName loc) = Atom (f x) fileName loc;
    };

    instance Applicative Atom where {
        {- 
        - Pure is not typically useful because the whole point of Atom 
        - is that we have information.  Pure is used under the hood,
        - and is typically used at the end of a chain of fmaps, where 
        - left operands contain atom data, overriding the pure empty atom.
        - We could theoretically grab info from the first element 
        - of the [Atom Char] input, however this is incorrect 
        - since x could originate from somewhere
        - not at the beginning of [Atom Char]
        -}
        pure x = Atom x "" (0, 0);
        (Atom f fileName pos) <*> (Atom x _ _) = Atom (f x) fileName pos;
    };

    instance Show a => Show (Atom a) where {
        show (Atom a fileName (row, col)) =
            show fileName ++ ":"  ++
            show row      ++ ":"  ++
            show col      ++ ": " ++
            show a;
    };

    atomize :: [FileName] -> [String] -> [Atom Char];
    atomize filePaths srcs = concatMap
        (uncurry (`atomizeRec` (1, 1)))
        (zip filePaths srcs);

    -- internal function for providing positions to each atom
    atomizeRec :: FileName -> (Row, Col) -> String -> [Atom Char];
    atomizeRec fileName (row, col) (x:xs)
        | x == '\n' = Atom x fileName (row, col) :
                    atomizeRec fileName (row + 1, 1) xs
        | otherwise = Atom x fileName (row, col) :
                    atomizeRec fileName (row, col + 1) xs;
    atomizeRec _ _ [] = [];

    newtype Lexer a = Lexer {
        runLexer :: [Atom Char] -> Maybe ([Atom Char], a)
    };

    instance Functor Lexer where {
        fmap f (Lexer x) = Lexer $ \input -> do {
            (input', y) <- x input;
            return (input', f y);
        };
    };

    instance Applicative Lexer where {
        pure x = Lexer $ \input -> Just (input, x);
        (Lexer x) <*> (Lexer y) = Lexer $ \input -> do {
            (input', f) <- x input;
            (input'', a) <- y input';
            return (input'', f a);
        };
    };

    instance Alternative Lexer where {
        empty = Lexer $ const Nothing;
        (Lexer x) <|> (Lexer y) = Lexer $ \input -> x input <|> y input;
    };

    instance Monad Lexer where {
        (Lexer x) >>= f = Lexer $ \input -> do {
            (rest, y) <- x input;
            runLexer (f y) rest;
        };
    };

    lexSpan :: (Char -> Bool) -> Lexer (Atom String);
    lexSpan p = Lexer $ \input -> let {
        (tkn, rest) = span (p . atomValue) input;
    } in Just (rest, sequenceA tkn);

    lexStr :: String -> Lexer (Atom String);
    lexStr = fmap sequenceA . traverse lexChar;

    lexChar :: Char -> Lexer (Atom Char);
    lexChar char = Lexer f where {
        f (x:xs)
            | char == atomValue x = Just (xs, x)
            | otherwise           = Nothing;
        f []     = Nothing;
    };

    lexFilter :: (a -> Bool) -> Lexer a -> Lexer a;
    lexFilter p (Lexer x) = Lexer $ \input -> do {
        (rest, x') <- x input;
        if p x' then
            return (rest, x');
        else
            empty;
    };

    specialChars :: [Char];
    specialChars = "(){}[]=$+-*&^";

    ws :: Lexer (Atom String);
    ws = lexSpan isSpace;

    tknKwrdFor :: Lexer (Atom Tkn);
    tknKwrdFor = do {
        kwrd <- lexStr "for";
        return (Keyword For <$ kwrd);
    };

    tknKwrdIn :: Lexer (Atom Tkn);
    tknKwrdIn = do {
        kwrd <- lexStr "in";
        return (Keyword In <$ kwrd);
    };

    tknKwrdCase :: Lexer (Atom Tkn);
    tknKwrdCase = do {
        kwrd <- lexStr "case";
        return (Keyword Case <$ kwrd);
    };

    tknKwrdLet :: Lexer (Atom Tkn);
    tknKwrdLet = do {
        kwrd <- lexStr "let";
        return (Keyword Let <$ kwrd);
    };

    tknKwrdStart :: Lexer (Atom Tkn);
    tknKwrdStart = do {
        kwrd <- lexStr "start";
        return (Keyword Start <$ kwrd);
    };

    tknKwrdWith :: Lexer (Atom Tkn);
    tknKwrdWith = do {
        kwrd <- lexStr "with";
        return (Keyword With <$ kwrd);
    };

    tknArrow :: Lexer (Atom Tkn);
    tknArrow = tknLeftArrow <|> tknRightArrow where {
        stringToDir string dir = do {
            arrowString <- lexStr string;
            return (Arrow dir <$ arrowString);
        };
        tknLeftArrow  = stringToDir "<-" L;
        tknRightArrow = stringToDir "->" R;
    };

    tknString :: Lexer (Atom Tkn);
    --TODO: Account for Escape Characters
    tknString = do {
        string <- stringPattern;
        return $ StringLiteral <$> string;
    } where {
        stringPattern = lexChar '\"' *> lexSpan (/= '\"') <* lexChar '\"'
    };

    tknOpenParen :: Lexer (Atom Tkn);
    tknOpenParen = do {
        char <- lexChar '(';
        return (OpenParen <$ char);
    };

    tknCloseParen :: Lexer (Atom Tkn);
    tknCloseParen = do {
        char <- lexChar ')';
        return (CloseParen <$ char);
    };

    tknOpenBrace :: Lexer (Atom Tkn);
    tknOpenBrace = do {
        char <- lexChar '{';
        return (OpenBrace <$ char);
    };

    tknCloseBrace :: Lexer (Atom Tkn);
    tknCloseBrace = do {
        char <- lexChar '}';
        return (CloseBrace <$ char);
    };

    tknOpenBracket :: Lexer (Atom Tkn);
    tknOpenBracket = do {
        char <- lexChar '[';
        return (OpenBracket <$ char);
    };

    tknCloseBracket :: Lexer (Atom Tkn);
    tknCloseBracket = do {
        char <- lexChar ']';
        return (CloseBracket <$ char);
    };

    tknAssign :: Lexer (Atom Tkn);
    tknAssign =do {
        char <- lexChar '=';
        return (Assign <$ char);
    };

    tknBinOpUnion :: Lexer (Atom Tkn);
    tknBinOpUnion = do {
        char <- lexChar '+';
        return (op <$ char);
    } where {
        op = BinaryOperation Union;
    };

    tknBinOpDifference :: Lexer (Atom Tkn);
    tknBinOpDifference = do {
        char <- lexChar '-';
        return (op <$ char);
    } where  {
        op = BinaryOperation Difference
    };

    tknBinOpCartesianProduct :: Lexer (Atom Tkn);
    tknBinOpCartesianProduct = do {
        char <- lexChar '*';
        return (op <$ char);
    } where  {
        op = BinaryOperation CartesianProduct
    };

    tknUnOpPower :: Lexer (Atom Tkn);
    tknUnOpPower = do {
        char <- lexChar '$';
        return (op <$ char);
    } where  {
        op = UnaryOperation Power
    };

    inRange :: Ord a => a -> (a, a) -> Bool;
    x `inRange` (a, b) = x >= a && x <= b;

    tknNum :: Lexer (Atom Tkn);
    tknNum = do { 
        num <- lexDecNum <|> lexBinNum <|> lexOctNum <|> lexHexNum;
        return $ Num <$> num;
    } where {
        lexDecNum = do {
            decNum <- lexSpan isDigit;
            case readMaybe $ atomValue decNum of {
                Just num -> return decNum {atomValue = num};
                Nothing  -> empty;
            };
        };
        lexBinNum = do {
            _ <- lexChar '0';
            _ <- lexChar 'b';
            binNum <- lexSpan (`elem` "01");
            let {
                parsed = fmap (convertBinToDec . reverse) binNum;
            };
            case atomValue parsed of {
                Just num -> return parsed {atomValue = num};
                Nothing -> empty;
            };
        } where {
            convertBinToDec = convertBaseToDec '0' '1' 1;
        };
        lexOctNum = do {
            _ <- lexChar '0';
            _ <- lexChar 'o';
            octNum <- lexSpan ((`elem` ['0'..'7']) . toLower);
            let {
                parsed = fmap (convertOctToDec . reverse) octNum;
            };
            case atomValue parsed of {
                Just num -> return parsed {atomValue = num};
                Nothing -> empty;
            };
        } where {
            convertOctToDec = convertBaseToDec '0' '7' 3;
        };
        lexHexNum = do {
            _ <- lexChar '0';
            _ <- lexChar 'x';
            hexNum <- lexSpan ((||) <$> (`elem` ['0'..'9']) <*> (`elem` ['a'..'f']) . toLower);
            let {
                parsed = fmap (convertHexToDec . reverse . map toLower) hexNum;
            };
            case atomValue parsed of {
                Just num -> return parsed {atomValue = num};
                Nothing -> empty;
            };
        } where {
            convertHexToDec [x]    | x `inRange` ('0', '9') = Just $ ord x - ord '0';
            convertHexToDec [x]    | x `inRange` ('a', 'f') = Just $ ord x - ord 'a' + 10;
            convertHexToDec (x:xs) | x `inRange` ('0', '9') = (+ (ord x - ord '0')) . (`shiftL` 4) <$> convertHexToDec xs;
            convertHexToDec (x:xs) | x `inRange` ('a', 'f') = (+ (ord x - ord 'a' + 10)) . (`shiftL` 4) <$> convertHexToDec xs;
            convertHexToDec _ = Nothing;
        };
        convertBaseToDec l h _ [x]        | x `inRange` (l, h) = Just $ ord x - ord l;
        convertBaseToDec l h shift (x:xs) | x `inRange` (l, h) = (+ (ord x - ord l)) . (`shiftL` shift) <$> convertBaseToDec l h shift xs;
        convertBaseToDec _ _ _ _ = Nothing;
    };

    tknWord :: Lexer (Atom Tkn);
    tknWord = do {
        validWord <- lexValidWord;
        return $ Word <$> validWord;
    } where {
        lexValidWord = lexFilter (not . null . atomValue) (lexSpan isValidChar);
        isValidChar char = all ($ char) [
            not . isSpace,
            not . flip elem specialChars
        ]
    };

    tknInvalid :: Lexer (Atom Tkn);
    tknInvalid = Lexer f where {
        f (x:xs) = Just (xs, x { atomValue = Invalid });
        f [] = Nothing;
    };

    lexTkn :: Lexer (Atom Tkn);
    lexTkn = asum [
        tknKeywords, tknDelimiters, tknOperations, tknMultiCharacter, tknInvalid
    ] where {
        tknKeywords = asum [
            tknKwrdFor, tknKwrdIn, tknKwrdCase, tknKwrdLet, tknKwrdStart, tknKwrdWith
        ];
        tknDelimiters = asum [
            tknOpenParen, tknCloseParen,
            tknOpenBrace, tknCloseBrace,
            tknOpenBracket, tknCloseBracket
        ];
        tknOperations = asum [
            tknAssign, tknArrow,
            tknUnOpPower,
            tknBinOpUnion, tknBinOpDifference, tknBinOpCartesianProduct
        ];
        tknMultiCharacter = asum [tknNum, tknString, tknWord]
    };

    lex :: [Atom Char] -> [Atom Tkn] -> Maybe [Atom Tkn];
    lex [] tkns = Just tkns;
    lex input tkns = do {
        (input', tkn) <- runLexer (ws *> lexTkn <* ws) input;
        if null input' then do {
            let {
                (Atom _ fileName (row, _)) = tkn;
                eof = Atom EndOfFile fileName (row + 1, 1);
            };
            return $ tkns ++ [tkn, eof];
        } else do {
            Lexer.lex input' (tkns ++ [tkn]);
        };
    };
}