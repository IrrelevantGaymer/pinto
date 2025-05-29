module Lexer where {
    import Control.Applicative ( Alternative((<|>), empty) );
    import Data.Char ( isSpace );

    import Tokens;
    import Data.Functor.Compose (Compose(..));
    import Data.Functor ((<&>));

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
    tknKwrdFor = getCompose $ Keyword For <$ Compose (lexStr "for");

    tknKwrdIn :: Lexer (Atom Tkn);
    tknKwrdIn = getCompose $ Keyword In <$ Compose (lexStr "in");

    tknKwrdCase :: Lexer (Atom Tkn);
    tknKwrdCase = getCompose $ Keyword Case <$ Compose (lexStr "case");

    tknKwrdLet :: Lexer (Atom Tkn);
    tknKwrdLet = getCompose $ Keyword Let <$ Compose (lexStr "let");

    tknKwrdStart :: Lexer (Atom Tkn);
    tknKwrdStart = getCompose $ Keyword Start <$ Compose (lexStr "start");

    tknKwrdWith :: Lexer (Atom Tkn);
    tknKwrdWith = getCompose $ Keyword With <$ Compose (lexStr "with");

    tknArrow :: Lexer (Atom Tkn);
    tknArrow = tknLeftArrow <|> tknRightArrow where {
        stringToDir string dir = getCompose $ Arrow dir <$ Compose (lexStr string);
        tknLeftArrow  = stringToDir "<-" L;
        tknRightArrow = stringToDir "->" R;
    };

    tknString :: Lexer (Atom Tkn);
    tknString = getCompose $ Compose stringPattern <&> StringLiteral where {
        stringPattern = lexChar '\"' *> lexSpan (/= '\"') <* lexChar '\"'
    };

    tknOpenParen :: Lexer (Atom Tkn);
    tknOpenParen = getCompose $ OpenParen <$ Compose (lexChar '(');

    tknCloseParen :: Lexer (Atom Tkn);
    tknCloseParen = getCompose $ CloseParen <$ Compose (lexChar ')');

    tknOpenBrace :: Lexer (Atom Tkn);
    tknOpenBrace = getCompose $ OpenBrace <$ Compose (lexChar '{');

    tknCloseBrace :: Lexer (Atom Tkn);
    tknCloseBrace = getCompose $ CloseBrace <$ Compose (lexChar '}');

    tknOpenBracket :: Lexer (Atom Tkn);
    tknOpenBracket = getCompose $ OpenBracket <$ Compose (lexChar '[');

    tknCloseBracket :: Lexer (Atom Tkn);
    tknCloseBracket = getCompose $ CloseBracket <$ Compose (lexChar ']');

    tknAssign :: Lexer (Atom Tkn);
    tknAssign = getCompose $ Assign <$ Compose (lexChar '=');

    tknBinOpUnion :: Lexer (Atom Tkn);
    tknBinOpUnion = getCompose $ op <$ Compose (lexChar '+') where {
        op = BinaryOperation Union;
    };

    tknBinOpDifference :: Lexer (Atom Tkn);
    tknBinOpDifference = getCompose $ op <$ Compose (lexChar '-') where {
        op = BinaryOperation Difference
    };

    tknBinOpCartesianProduct :: Lexer (Atom Tkn);
    tknBinOpCartesianProduct = getCompose $ op <$ Compose (lexChar '*') where {
        op = BinaryOperation CartesianProduct
    };

    tknUnOpPower :: Lexer (Atom Tkn);
    tknUnOpPower = getCompose $ op <$ Compose (lexChar '$') where {
        op = UnaryOperation Power
    };

    tknWord :: Lexer (Atom Tkn);
    tknWord = getCompose $ Word <$> Compose lexValidWord where {
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
    lexTkn = tknKwrdFor   <|> tknKwrdIn     <|> tknKwrdCase    <|> tknKwrdLet      <|>
             tknKwrdStart <|> tknKwrdWith   <|> tknOpenParen   <|> tknCloseParen   <|>
             tknOpenBrace <|> tknCloseBrace <|> tknOpenBracket <|> tknCloseBracket <|>
             tknAssign    <|> tknArrow      <|> tknString      <|> tknWord;

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