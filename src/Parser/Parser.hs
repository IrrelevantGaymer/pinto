module Parser where

import Tokens
import Lexer

import ParserError

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
