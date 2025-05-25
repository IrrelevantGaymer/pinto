module Parser where

import Control.Applicative
    ( Alternative(empty), Alternative(many) )

import Tokens ( Tkn, Token(CloseParen, Word, OpenParen, Arrow, Keyword, OpenBracket, CloseBracket, Assign, EndOfFile), Arrow (..), Keyword (..) )
import Lexer ( Atom(Atom, atomValue) )

import ParserError ( ParserError(ExpectedWord, UnexpectedToken, Empty) )
import AST ( Pattern(..), Dir, Direction(..), Rule (Rule), Tape (..), Node (..), AST (..) )
import GHC.Base (Alternative((<|>)))

newtype Parser a = Parser {
    runParser :: [Atom Tkn] -> Either ParserError ([Atom Tkn], a)
}

instance Functor Parser where
    fmap f (Parser x) = Parser $ \input -> do
        (input', y) <- x input
        return (input', f y)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (input, x)
    (Parser x) <*> (Parser y) = Parser $ \input -> do
        (input', f) <- x input
        (input'', a) <- y input'
        return (input'', f a)

instance Alternative Parser where
    empty = Parser $ const $ Left Empty
    (Parser x) <|> (Parser y) = Parser $ \input -> f (x input) (y input)
        where
            f (Left a)  (Left _)  = Left  a
            f (Right a) _         = Right a
            f _         (Right a) = Right a

nodeTkns :: [Tkn] -> Parser (Atom [Tkn])
nodeTkns = fmap sequenceA . traverse nodeTkn

nodeTkn :: Tkn -> Parser (Atom Tkn)
nodeTkn tkn = Parser f
    where f (x:xs) = if tkn == atomValue x then
                        Right (xs, x)
                     else
                        Left (UnexpectedToken tkn x)
          -- TODO: encode this unreachableness in the type system
          f [] = error "Unreachable: list of tokens should always end with EndOfFile"

nodeDir :: Parser (Atom Dir)
nodeDir = leftDir <|> rightDir
    where
        leftDir  = (AST.L <$) <$> nodeTkn (Arrow Tokens.L)
        rightDir = (AST.R <$) <$> nodeTkn (Arrow Tokens.R)

nodeWord :: Parser (Atom String)
nodeWord = Parser f
    where f (x:xs) = case x of
            Atom (Word value) fileName pos -> Right (xs, Atom value fileName pos)
            _ -> Left (ExpectedWord x)
          f [] = error "Unreachable: list of tokens should always end with EndOfFile"

nodeTape :: Parser Node
nodeTape = fmap (atomValue . (TapeNode <$>)) (fmap ($ 0) <$> (fmap Tape
    <$>  (nodeTkn (Keyword Start) *> nodeWord)
    <**> (nodeTkn (Keyword With)  *> nodePattern <* nodeTkn Assign)
    <**> nodePatternList))
    where
        infixl 4 <**>
        (<**>) :: (Applicative f1, Applicative f2) =>
            f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
        a <**> b = (<*>) <$> a <*> b


nodeBasicRule :: Parser Node
nodeBasicRule = fmap (atomValue . (RuleNode <$>)) (fmap Rule
    <$>  (nodeTkn (Keyword Case) *>  nodePattern)
    <**> nodePattern
    <**> nodePattern
    <**> nodeDir
    <**> nodePattern)
    where
        infixl 4 <**>
        (<**>) :: (Applicative f1, Applicative f2) =>
            f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
        a <**> b = (<*>) <$> a <*> b


nodePattern :: Parser (Atom Pattern)
nodePattern = nodePatternValue <|> fmap List <$> nodePatternList <|> nodePatternTuple

nodePatternValue :: Parser (Atom Pattern)
nodePatternValue = fmap Value <$> nodeWord

nodePatternList :: Parser (Atom [Pattern])
nodePatternList = fmap sequenceA
    (nodeTkn OpenBracket *> many nodePattern <* nodeTkn CloseBracket)

nodePatternTuple :: Parser (Atom Pattern)
nodePatternTuple = fmap Tuple <$> fmap sequenceA
    (nodeTkn OpenParen *> many nodePattern <* nodeTkn CloseParen)

parseNode :: Parser Node
parseNode = nodeTape <|> nodeBasicRule

parse :: [Atom Tkn] -> AST -> Either ParserError AST
parse [Atom EndOfFile _ _] ast = Right ast
parse input (AST tapes rules) = do
    (input', node) <- runParser parseNode input
    {- 
     - I *REALLY* love Haskell, but it took me 15 minutes to realize 
     - that TapeNode and RuleNode had to be indented twice.  This is why
     - whitespace dependent languages are annoying.  If Haskell was
     - not whitespace dependent, *chef's kiss* magnifico.
     - Next Project Idea Maybe??
     -}
    let ast' = case node of
            TapeNode tape -> AST (tape : tapes) rules
            RuleNode rule -> AST tapes (rule : rules)
    parse input' ast'

