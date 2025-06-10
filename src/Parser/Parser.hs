module Parser where {
    import Control.Applicative ( Alternative(empty), Alternative(many) );

    import Tokens (
        Tkn,
        Token(..),
        Arrow (..),
        Keyword (..), BinaryOperation (..), UnaryOperation (Power)
    );
    import Lexer ( Atom(Atom, atomValue) );

    import ParserError ( ParserError(..), ExpectForCase (CurrentState, FromValue, ToValue, CaseDir, NextState), ExpectForUQ (..), ExpectForSetDef (..) );
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
    import FullResult (FullResult (..), injectMapHard, FullFunctor (..), FullMonad (..), FullApplicative (..), (<|&|>), (<:&:>));;;;;;;;
    import Data.Foldable (asum);

    newtype Parser s h a = Parser {
        runParser :: [Atom Tkn] -> FullResult s h ([Atom Tkn], a)
    };

    (<|=:$) :: Parser s h a -> (s -> h) -> Parser s h a;
    infixl 4 <|=:$;
    (Parser x) <|=:$ err = Parser $ \input -> x input `injectMapHard` err;

    instance Functor (Parser s h) where {
        fmap f (Parser x) = Parser $ \input -> do {
            (input', y) <- x input;
            return (input', f y);
        };
    };

    instance FullFunctor Parser where {
        sfmap f (Parser x) = Parser $
            \input -> x input >>:=
            \y     -> sreturn $ f y;
        hfmap f (Parser x) = Parser $
            \input -> x input >>|=
            \y     -> hreturn $ f y;
    };

    instance Applicative (Parser s h) where {
        pure x = Parser $ \input -> Ok (input, x);
        (Parser x) <*> (Parser y) = Parser $ \input -> do {
            (input', f) <- x input;
            (input'', a) <- y input';
            return (input'', f a);
        };
    };

    instance FullApplicative Parser where {
        spure x = Parser $ const $ Soft x;
        hpure x = Parser $ const $ Hard x;

        (Parser x) <:*:> (Parser y) = Parser $ \input ->
            x input >>:= \f ->
            y input >>:= \a ->
            sreturn $ f a;
        (Parser x) <|*|> (Parser y) = Parser $ \input ->
            x input >>|= \f ->
            y input >>|= \a ->
            hreturn $ f a;
    };

    instance Alternative (Parser s h) where {
        empty = Parser $ const Empty;
        (Parser x) <|> (Parser y) = Parser $ \input -> x input <|> y input;
    };

    instance Monad (Parser s h) where {
        (Parser x) >>= f = Parser $ \input -> do {
            (rest, y) <- x input;
            runParser (f y) rest
        };
    };

    nodeTkns :: [Tkn] -> Parser ParserError ParserError (Atom [Tkn]);
    nodeTkns tkns = do {
        parsedTkns <- traverse nodeTkn tkns;
        return $ sequenceA parsedTkns;
    };

    nodeTkn :: Tkn -> Parser ParserError ParserError (Atom Tkn);
    nodeTkn tkn = Parser f where {
        f (x:xs)
            | tkn == atomValue x = Ok (xs, x)
            | otherwise          = Soft (UnexpectedToken tkn x);
        -- TODO: encode this unreachableness in the type system
        f [] = error "Unreachable: list of tokens should always end with EndOfFile";
    };

    nodeDir :: Parser ParserError ParserError (Atom Dir);
    nodeDir = leftDir <|> rightDir where {
        arrowToDir arrow dir = do {
            arrowTkn <- nodeTkn $ Arrow arrow;
            return (dir <$ arrowTkn);
        };
        leftDir  = arrowToDir Tokens.L Dir.L;
        rightDir = arrowToDir Tokens.R Dir.R;
    };

    nodeWord :: Parser ParserError ParserError (Atom String);
    nodeWord = Parser f where {
        f (x:xs)
            | Atom (Tokens.Word value) fileName pos <- x = Ok (xs, Atom value fileName pos)
            | otherwise                                  = Soft (ExpectedWord x);
        f [] = error "Unreachable: list of tokens should always end with EndOfFile";
    };

    nodeTape :: Parser ParserError ParserError Node;
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

    nodeBasicRuleNode :: Parser ParserError ParserError Node;
    nodeBasicRuleNode = RuleNode . SimpleRule <$> nodeBasicRule;

    nodeBasicRule :: Parser ParserError ParserError BasicRule;
    nodeBasicRule = do {
        startBasicRule <- nodeTkn $ Keyword Case;
        currentState   <- atomValue <$> nodePat <|=:$ ExpectedForCase startBasicRule CurrentState . received;
        fromValue      <- atomValue <$> nodePat <|=:$ ExpectedForCase startBasicRule FromValue    . received;
        toValue        <- atomValue <$> nodePat <|=:$ ExpectedForCase startBasicRule ToValue      . received;
        dir            <- atomValue <$> nodeDir <|=:$ ExpectedForCase startBasicRule CaseDir      . received;
        nextState      <- atomValue <$> nodePat <|=:$ ExpectedForCase startBasicRule NextState    . received;
        return $ BasicRule
            currentState fromValue toValue dir nextState;
    };

    nodeUQRuleNode :: Parser ParserError ParserError Node;
    nodeUQRuleNode = RuleNode . ComplexRule <$> nodeUQRule;

    nodeUQRule :: Parser ParserError ParserError UQRule;
    nodeUQRule = do {
        startForRule <- nodeTkn $ Keyword For;
        uqPat        <- atomValue <$> nodePat <|=:$ ExpectedForUQ startForRule Pat    . received;
        _            <- nodeTkn (Keyword In)  <|=:$ ExpectedForUQ startForRule ForIn  . received;
        uqPatSet     <- atomValue <$> nodeSet <|=:$ ExpectedForUQ startForRule PatSet . received;

        -- TODO type check uqPat
        uqRules <- block startForRule <|> singleton <$> single;
        return $ UniversalQuantifierRule uqPat uqPatSet uqRules;
    } where {
        block startForRule = do {
            open <- nodeTkn OpenBrace;
            rules <- many single    <|=:$ dupApply (ExpectedRuleForUQ startForRule . received)
                                    <|&|> dupApply (ExpectedRuleForUQ startForRule . received);
            _ <- nodeTkn CloseBrace <|=:$ ExpectedClose open CloseBrace   . received;
            return rules;
        };
        single = SimpleRule <$> nodeBasicRule <|> ComplexRule <$> nodeUQRule;
        singleton a = [a];
        dupApply f a = f a a;
    };

    nodeSetNode :: Parser ParserError ParserError Node;
    nodeSetNode = do {
        startSetDef <- nodeTkn $ Keyword Let;
        name <- atomValue <$> nodeWord <|=:$ ExpectedForSetDef startSetDef Name         . received;
        _ <- nodeTkn Assign            <|=:$ ExpectedForSetDef startSetDef SetDefAssign . received;
        set <- atomValue <$> nodeSet;
        return $ SetNode (name, set);
    };

    nodeSet :: Parser ParserError ParserError (Atom SetDef);
    nodeSet = do {
        left <- nodeAtomSet;
        parseSetExpr left <|> return (unwrapId <$> left);
    } where {
        unwrapId (Id s) = unwrapId s;
        unwrapId nonId = nonId;
    };

    parseSetExpr :: Atom SetDef -> Parser ParserError ParserError (Atom SetDef);
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

    nodeAtomSet :: Parser ParserError ParserError (Atom SetDef);
    nodeAtomSet = do {
        set <- nodeGroupSet <|> nodeSetWithUnary <|> nodeBasicSet <|> nodeWordSet;
        return $ Id <$> set;
    } where {
        nodeWordSet = do {
            set <- nodeWord;
            return $ Sets.Word <$> set;
        };
        nodeGroupSet = do {
            open <- nodeTkn OpenParen;
            set  <- nodeSet;
            _    <- nodeTkn CloseParen <|=:$ ExpectedClose open CloseParen . received;
            return set;
        };
        nodeSetWithUnary = do {
            op <- nodeUnaryOp;
            atomSet <- nodeAtomSet;
            return $ Sets.UnOpSet <$> op <*> atomSet;
        };
    };

    nodeBasicSet :: Parser ParserError ParserError (Atom SetDef);
    nodeBasicSet = do {
        patterns <- sequenceA <$> setPat;
        return $ Set <$> patterns;
    } where {
        setPat = do {
            open <- nodeTkn OpenBrace;
            pats <- many nodePat       <|=:$ ExpectedPatInBasicSet open    . received;
            _    <- nodeTkn CloseBrace <|=:$ ExpectedClose open CloseBrace . received;
            return pats;
        };
    };

    nodeBinaryOp :: Parser ParserError ParserError (Atom BinOp);
    nodeBinaryOp = (unionOp <|> differenceOp <|> cartesianProductOp) <:&:> ExpectedBinaryOp . received where {
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

    nodeUnaryOp :: Parser ParserError ParserError (Atom UnOp);
    nodeUnaryOp = powerOp <:&:> ExpectedUnaryOp  . received where {
        tknToUnOp tkn op = do {
            opTkn <- nodeTkn tkn;
            return (op <$ opTkn);
        };
        powerOp = tknToUnOp (UnaryOperation Tokens.Power) Sets.PowerSet;
    };

    nodePat :: Parser ParserError ParserError (Atom (Pat String));
    nodePat = (nodePatValue <|> nodePatListNode <|> nodePatTuple) <:&:> CouldNotParsePattern . received where {
        nodePatListNode = do {
            list <- nodePatList;
            return $ List <$> list;
        };
    };

    nodePatValue :: Parser ParserError ParserError (Atom (Pat String));
    nodePatValue = do {
        value <- nodeWord;
        return $ Value <$> value;
    };

    nodePatList :: Parser ParserError ParserError (Atom [Pat String]);
    nodePatList = sequenceA <$> listPat where {
        listPat = do {
            open <- nodeTkn OpenBracket;
            pats <- many nodePat         <|=:$ id;
            _    <- nodeTkn CloseBracket <|=:$ ExpectedClose open CloseBracket . received;
            return pats;
        };
    };

    nodePatTuple :: Parser ParserError ParserError (Atom (Pat String));
    nodePatTuple = do {
        patterns <- sequenceA <$> tuplePat;
        return $ Tuple <$> patterns;
    } where {
        tuplePat = do {
            open <- nodeTkn OpenParen;
            pats <- many nodePat       <|=:$ id;
            _    <- nodeTkn CloseParen <|=:$ ExpectedClose open CloseParen . received;
            return pats;
        };
    };

    parseNode :: Parser ParserError ParserError Node;
    parseNode = asum [
        nodeSetNode,
        nodeTape,
        nodeBasicRuleNode,
        nodeUQRuleNode
    ] <|=:$ ExpectedNode . received;

    parse :: [Atom Tkn] -> AST -> FullResult ParserError ParserError AST;
    parse [Atom EndOfFile _ _] ast = Ok ast;
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