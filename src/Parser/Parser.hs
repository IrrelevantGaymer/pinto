module Parser where

import Control.Applicative
    ( Alternative(empty), Alternative(many) )

import Tokens ( Tkn, Token(CloseParen, Word, OpenParen, Arrow, Keyword), Arrow (..), Keyword (Case) )
import Lexer ( Atom(Atom, atomValue) )

import ParserError ( ParserError(ExpectedWord, UnexpectedToken, Empty) )
import AST ( Pattern(Value, Tuple), Dir, Direction(..), Rule (Rule) )
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

parseTkns :: [Tkn] -> Parser (Atom [Tkn])
parseTkns = fmap sequenceA . traverse parseTkn

parseTkn :: Tkn -> Parser (Atom Tkn)
parseTkn tkn = Parser f
    where f (x:xs) = if tkn == atomValue x then
                        Right (xs, x)
                     else
                        Left (UnexpectedToken tkn x)
          -- TODO: encode this unreachableness in the type system
          f [] = error "Unreachable: list of tokens should always end with EndOfFile"

parseDir :: Parser (Atom Dir)
parseDir = leftDir <|> rightDir
    where
        leftDir = (AST.L <$) <$> parseTkn (Arrow Tokens.L)
        rightDir = (AST.R <$) <$> parseTkn (Arrow Tokens.R)

parseWord :: Parser (Atom String)
parseWord = Parser f
    where f (x:xs) = case x of
            Atom (Word value) fileName pos -> Right (xs, Atom value fileName pos)
            _ -> Left (ExpectedWord x)
          f [] = error "Unreachable: list of tokens should always end with EndOfFile"

parseBasicRule :: Parser (Atom Rule)
parseBasicRule = parseTkn (Keyword Case) *> (fmap Rule <$> parsePattern 
    <**> parsePattern 
    <**> parsePattern 
    <**> parseDir 
    <**> parsePattern)
    where
        infixl 4 <**>
        (<**>) :: (Applicative f1, Applicative f2) => f1 (f2 (a -> b)) -> f1 (f2 a) -> f1 (f2 b)
        a <**> b = (<*>) <$> a <*> b


parsePattern :: Parser (Atom Pattern)
parsePattern = parsePatternValue <|> parsePatternTuple

parsePatternValue :: Parser (Atom Pattern)
parsePatternValue = fmap Value <$> parseWord

parsePatternTuple :: Parser (Atom Pattern)
parsePatternTuple = fmap Tuple <$> fmap sequenceA
    (parseTkn OpenParen *> many parsePattern <* parseTkn CloseParen)
