module Parser where {
    import Control.Applicative ( Alternative(empty), Alternative(many) );

    import Tokens (
        Tkn,
        Token(..),
        Arrow (..),
        Keyword (..), BinaryOperation (..), UnaryOperation (Power)
    );
    import Lexer ( Atom(Atom, atomValue) );

    import ParserError ( ParserError(..) );
    import AST (
        Node (..),
        AST (..)
    );
    import GHC.Base (Alternative((<|>)));
    import Direction (Dir);
    import qualified Direction as Dir;
    import Pattern (Pat, Pattern (..));
    import Rule (Rule(..), BasicRule (..), UQRule, UniversalQuantifierRule (UniversalQuantifierRule));
    import Tape (Tape(..));
    import Sets (BinOp, UnOp, SetDef (..), BinarySetOperation (..), UnarySetOperation (..), getPrecedence);
    import Text.Printf (printf);
    
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

    instance Monad Parser where {
        (Parser x) >>= f = Parser $ \input -> do {
            (rest, y) <- x input;
            runParser (f y) rest
        };
    };

    nodeTkns :: [Tkn] -> Parser (Atom [Tkn]);
    nodeTkns tkns = do {
        parsedTkns <- traverse nodeTkn tkns;
        return $ sequenceA parsedTkns;
    };

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
        arrowToDir arrow dir = do {
            arrowTkn <- nodeTkn $ Arrow arrow;
            return (dir <$ arrowTkn);
        };
        leftDir  = arrowToDir Tokens.L Dir.L;
        rightDir = arrowToDir Tokens.R Dir.R;
    };

    nodeWord :: Parser (Atom String);
    nodeWord = Parser f where {
        f (x:xs)
            | Atom (Tokens.Word value) fileName pos <- x = Right (xs, Atom value fileName pos)
            | otherwise                                  = Left (ExpectedWord x);
        f [] = error "Unreachable: list of tokens should always end with EndOfFile";
    };

    nodeTape :: Parser Node;
    nodeTape = do {
        _          <- nodeTkn $ Keyword Start;
        name       <- atomValue <$> nodeWord;
        _          <- nodeTkn $ Keyword With;
        startState <- atomValue <$> nodePat;
        _          <- nodeTkn Assign;
        values     <- atomValue <$> nodePatList;
        -- TODO: Get Initial index
        -- Will probably look like (values, index) <- ...
        return $ TapeNode $ Tape name startState values 0;
    };

    nodeBasicRuleNode :: Parser Node;
    nodeBasicRuleNode = RuleNode . SimpleRule <$> nodeBasicRule;

    nodeBasicRule :: Parser BasicRule;
    nodeBasicRule = do {
        _            <- nodeTkn $ Keyword Case;
        currentState <- atomValue <$> nodePat;
        fromValue    <- atomValue <$> nodePat;
        toValue      <- atomValue <$> nodePat;
        dir          <- atomValue <$> nodeDir;
        nextState    <- atomValue <$> nodePat;
        return $ BasicRule
            currentState fromValue toValue dir nextState;
    };

    nodeUQRuleNode :: Parser Node;
    nodeUQRuleNode = RuleNode . ComplexRule <$> nodeUQRule;

    nodeUQRule :: Parser UQRule;
    nodeUQRule = do {
        _ <- nodeTkn $ Keyword For;
        uqPat <- atomValue <$> nodePat;
        _ <- nodeTkn $ Keyword In;
        uqPatSet <- atomValue <$> nodeSet;

        -- TODO type check uqPat
        uqRules <- block <|> singleton <$> single;
        return $ UniversalQuantifierRule uqPat uqPatSet uqRules;
    } where {
        block = do {
            _ <- nodeTkn OpenBrace;
            rules <- many single;
            _ <- nodeTkn CloseBrace;
            return rules;
        };
        single = SimpleRule <$> nodeBasicRule <|> ComplexRule <$> nodeUQRule;
        singleton a = [a];
    };

    nodeSetNode :: Parser Node;
    nodeSetNode = do {
        _ <- nodeTkn $ Keyword Let;
        name <- atomValue <$> nodeWord;
        _ <- nodeTkn Assign;
        set <- atomValue <$> nodeSet;
        return $ SetNode (name, set);
    };

    nodeSet :: Parser (Atom SetDef);
    nodeSet = do {
        left <- nodeAtomSet;
        parseSetExpr left <|> return (unwrapId <$> left);
    } where {
        unwrapId (Id s) = unwrapId s;
        unwrapId nonId = nonId;
    };

    parseSetExpr :: Atom SetDef -> Parser (Atom SetDef);
    parseSetExpr left = do {
        op <- nodeBinaryOp;
        right <- nodeAtomSet;
        let {
            left' = nestByPrecedence <$> left <*> op <*> right;
        };
        parseSetExpr left' <|> return left';
    };

    nestByPrecedence :: SetDef -> BinOp -> SetDef -> SetDef;
    nestByPrecedence (BinOpSet op1 s1 s2) op2 (Id s3)
        | prec2 >= prec1 = BinOpSet op2 (BinOpSet op1 s1 s2) s3
        | otherwise = BinOpSet op1 s1 (nestByPrecedence (Id s2) op2 (Id s3))
    where {
        prec1 = getPrecedence op1;
        prec2 = getPrecedence op2;
    };
    nestByPrecedence (Id s1) op (Id s2) = BinOpSet op s1 s2;
    nestByPrecedence a o b = error $
        printf "unreachable pattern: %s %s %s" (show a) (show o) (show b);

    nodeAtomSet :: Parser (Atom SetDef);
    nodeAtomSet = do {
        set <- nodeGroupSet <|> nodeSetWithUnary <|> nodeBasicSet <|> nodeWordSet;
        return $ Id <$> set;
    } where {
        nodeWordSet = do {
            set <- nodeWord;
            return $ Sets.Word <$> set;
        };
        nodeGroupSet = nodeTkn OpenParen *> nodeSet <* nodeTkn CloseParen;
        nodeSetWithUnary = do {
            op <- nodeUnaryOp;
            atomSet <- nodeAtomSet;
            return $ Sets.UnOpSet <$> op <*> atomSet;
        };
    };

    nodeBasicSet :: Parser (Atom SetDef);
    nodeBasicSet = do {
        patterns <- sequenceA <$> setPat;
        return $ Set <$> patterns;
    } where {
        setPat = nodeTkn OpenBrace *> many nodePat <* nodeTkn CloseBrace;
    };

    nodeBinaryOp :: Parser (Atom BinOp);
    nodeBinaryOp = unionOp <|> differenceOp <|> cartesianProductOp where {
        tknToBinOp tkn op = do {
            opTkn <- nodeTkn tkn;
            return (op <$ opTkn);
        };
        unionOp = tknToBinOp
            (BinaryOperation Tokens.Union)
            Sets.Union;
        differenceOp = tknToBinOp
            (BinaryOperation Tokens.Difference)
            Sets.Difference;
        cartesianProductOp = tknToBinOp
            (BinaryOperation Tokens.CartesianProduct)
            Sets.CartesianProduct;
    };

    nodeUnaryOp :: Parser (Atom UnOp);
    nodeUnaryOp = powerOp where {
        tknToUnOp tkn op = do {
            opTkn <- nodeTkn tkn;
            return (op <$ opTkn);
        };
        powerOp = tknToUnOp (UnaryOperation Tokens.Power) Sets.PowerSet;
    };

    nodePat :: Parser (Atom Pat);
    nodePat = nodePatValue <|> nodePatListNode <|> nodePatTuple where {
        nodePatListNode = do {
            list <- nodePatList;
            return $ List <$> list;
        };
    };

    nodePatValue :: Parser (Atom Pat);
    nodePatValue = do {
        value <- nodeWord;
        return $ Value <$> value;
    };

    nodePatList :: Parser (Atom [Pat]);
    nodePatList = sequenceA <$> listPat where {
        listPat = nodeTkn OpenBracket *> many nodePat <* nodeTkn CloseBracket;
    };

    nodePatTuple :: Parser (Atom Pat);
    nodePatTuple = do {
        patterns <- sequenceA <$> tuplePat;
        return $ Tuple <$> patterns;
    } where {
        tuplePat = nodeTkn OpenParen *> many nodePat <* nodeTkn CloseParen
    };

    parseNode :: Parser Node;
    parseNode = nodeSetNode <|> nodeTape <|> nodeBasicRuleNode <|> nodeUQRuleNode;

    parse :: [Atom Tkn] -> AST -> Either ParserError AST;
    parse [Atom EndOfFile _ _] ast = Right ast;
    parse input ast = do {
        (input', node) <- runParser parseNode input;
        let {
            (AST sets tapes rules) = ast;
            ast' = case node of {
                SetNode  set  -> ast {astSets =  set:sets };
                TapeNode tape -> ast {astTapes = tape:tapes };
                RuleNode rule -> ast {astRules = rule:rules };
            };
        };
        parse input' ast';
    };

}