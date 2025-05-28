module Parser where {
    import Control.Applicative ( Alternative(empty), Alternative(many) );

    import Tokens (
        Tkn,
        Token(..),
        Arrow (..),
        Keyword (..)
    );
    import Lexer ( Atom(Atom, atomValue) );

    import ParserError ( ParserError(..) );
    import AST (
        Pattern(..),
        Dir,
        Direction(..),
        Rule (..),
        Tape (..),
        Node (..),
        AST (..)
    );
    import GHC.Base (Alternative((<|>)));
    import Data.Functor.Compose (Compose(..));
    import Data.Functor ((<&>));

    newtype Parser a = Parser {
        runParser :: [Atom Tkn] -> Either ParserError ([Atom Tkn], a)
    };

    instance Functor Parser where {
        fmap f (Parser x) = Parser $ \input -> do {
            (input', y) <- x input;
            return (input', f y);
        };
    };

    instance Applicative Parser where {
        pure x = Parser $ \input -> Right (input, x);
        (Parser x) <*> (Parser y) = Parser $ \input -> do {
            (input', f) <- x input;
            (input'', a) <- y input';
            return (input'', f a);
        };
    };

    instance Alternative Parser where {
        empty = Parser $ const $ Left Empty;
        (Parser x) <|> (Parser y) = Parser $ f x y where {
            f u v input = g (u input) (v input);
            g (Left a)  (Left _)  = Left  a;
            g (Right a) _         = Right a;
            g _         (Right a) = Right a;
        };
    };

    nodeTkns :: [Tkn] -> Parser (Atom [Tkn]);
    nodeTkns = getCompose $ sequenceA <$> Compose (traverse nodeTkn);

    nodeTkn :: Tkn -> Parser (Atom Tkn);
    nodeTkn tkn = Parser f where {
        f (x:xs)
            | tkn == atomValue x = Right (xs, x)
            | otherwise          = Left (UnexpectedToken tkn x);
        -- TODO: encode this unreachableness in the type system
        f [] = error "Unreachable: list of tokens should always end with EndOfFile";
    };

    nodeDir :: Parser (Atom Dir);
    nodeDir = leftDir <|> rightDir where {
        arrowToDir arrow dir = getCompose $ dir <$ Compose (nodeTkn $ Arrow arrow);
        leftDir  = arrowToDir Tokens.L AST.L;
        rightDir = arrowToDir Tokens.R AST.R;
    };

    nodeWord :: Parser (Atom String);
    nodeWord = Parser f where {
        f (x:xs)
            | Atom (Word value) fileName pos <- x = Right (xs, Atom value fileName pos)
            | otherwise                           = Left (ExpectedWord x);
        f [] = error "Unreachable: list of tokens should always end with EndOfFile";
    };

    nodeTape :: Parser Node;
    nodeTape = atomValue <$> getCompose (Tape
        <$> Compose (nodeTkn (Keyword Start) *> nodeWord)
        <*> Compose (nodeTkn (Keyword With) *> nodePattern <* nodeTkn Assign)
        <*> Compose nodePatternList
        <&> ($ 0)
        <&> TapeNode
    );

    nodeBasicRule :: Parser Node;
    nodeBasicRule = atomValue <$> getCompose (Rule
        <$> Compose (nodeTkn (Keyword Case) *> nodePattern)
        <*> Compose nodePattern
        <*> Compose nodePattern
        <*> Compose nodeDir
        <*> Compose nodePattern
        <&> RuleNode
    );

    nodePattern :: Parser (Atom Pattern);
    nodePattern = nodePatternValue <|>
        fmap List <$> nodePatternList <|>
        nodePatternTuple;

    nodePatternValue :: Parser (Atom Pattern);
    nodePatternValue = getCompose $ Value <$> Compose nodeWord;

    nodePatternList :: Parser (Atom [Pattern]);
    nodePatternList = sequenceA <$> listPattern where {
        listPattern = nodeTkn OpenBracket *> many nodePattern <* nodeTkn CloseBracket;
    };

    nodePatternTuple :: Parser (Atom Pattern);
    nodePatternTuple = getCompose $ Tuple <$> Compose (sequenceA <$> tuplePattern) where {
        tuplePattern = nodeTkn OpenParen *> many nodePattern <* nodeTkn CloseParen
    };

    parseNode :: Parser Node;
    parseNode = nodeTape <|> nodeBasicRule;

    parse :: [Atom Tkn] -> AST -> Either ParserError AST;
    parse [Atom EndOfFile _ _] ast = Right ast;
    parse input (AST tapes rules) = do {
        (input', node) <- runParser parseNode input;
        let {
            ast' = case node of {
                TapeNode tape -> AST (tape : tapes) rules;
                RuleNode rule -> AST tapes (rule : rules);
            };
        };
        parse input' ast';
    }

}