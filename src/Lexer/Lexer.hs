module Lexer where

import Control.Applicative
import Data.Char

import Tokens

type FileName = String
type Row = Int
type Col = Int

data Atom a = Atom {
    atomValue    :: a,
    atomFileName :: FileName,
    atomPos      :: (Row, Col)
}

instance Functor Atom where
    fmap f (Atom x fileName loc) = Atom (f x) fileName loc

instance Applicative Atom where
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
    pure x = Atom x "" (0, 0)
    (Atom f fileName pos) <*> (Atom x _ _) = Atom (f x) fileName pos

instance Show a => Show (Atom a) where
    show (Atom a fileName (row, col)) = show fileName ++ ":" ++
        show row ++ ":"  ++
        show col ++ ": " ++
        show a

atomize :: [FileName] -> [String] -> [Atom Char]
atomize filePaths srcs = concatMap
    (uncurry (`atomizeRec` (1, 1)))
    (zip filePaths srcs)

-- internal function for providing positions to each atom
atomizeRec :: FileName -> (Row, Col) -> String -> [Atom Char]
atomizeRec fileName (row, col) (x:xs)
    | x == '\n' = Atom x fileName (row, col) :
                  atomizeRec fileName (row + 1, 1) xs
    | otherwise = Atom x fileName (row, col) :
                  atomizeRec fileName (row, col + 1) xs
atomizeRec _ _ [] = []

{- 
 - We do not restrict the output to Atom a because Atom 
 - being an applicative is not useful because we typically
 - need information to construct an Atom.
 - We might still have to make Atom an Applicative,
 - so we can do monadic operations on it later???
 -}
newtype Lexer a = Lexer {
    runLexer :: [Atom Char] -> Maybe ([Atom Char], a)
}

instance Functor Lexer where
    fmap f (Lexer x) = Lexer $ \input -> do
        (input', y) <- x input
        return (input', f y)

instance Applicative Lexer where
    pure x = Lexer $ \input -> Just (input, x)
    (Lexer x) <*> (Lexer y) = Lexer $ \input -> do
        (input', f) <- x input
        (input'', a) <- y input'
        return (input'', f a)

instance Alternative Lexer where
    empty = Lexer $ const Nothing
    (Lexer x) <|> (Lexer y) = Lexer $ \input -> x input <|> y input

lexSpan :: (Char -> Bool) -> Lexer (Atom String)
lexSpan p = Lexer $ \input ->
        let (tkn, rest) = span (p . atomValue) input
        in Just (rest, sequenceA tkn)

lexStr :: String -> Lexer (Atom String)
lexStr = fmap sequenceA . traverse lexChar

lexChar :: Char -> Lexer (Atom Char)
lexChar char = Lexer f
    where f (x:xs) = if char == atomValue x then
                        Just (xs, x)
                     else
                        Nothing
          f []     = Nothing

specialChars :: [Char]
specialChars = "(){}"

ws :: Lexer (Atom String)
ws = lexSpan isSpace

tknKwrdFor :: Lexer (Atom Tkn)
tknKwrdFor = (Keyword For <$) <$> lexStr "for"

tknKwrdIn :: Lexer (Atom Tkn)
tknKwrdIn = (Keyword In <$) <$> lexStr "in"

tknKwrdCase :: Lexer (Atom Tkn)
tknKwrdCase = (Keyword Case <$) <$> lexStr "case"

tknKwrdLet :: Lexer (Atom Tkn)
tknKwrdLet = (Keyword Let <$) <$> lexStr "let"

tknArrow :: Lexer (Atom Tkn)
tknArrow = tknLeftArrow <|> tknRightArrow
    where
        tknLeftArrow = (Arrow L <$) <$> lexStr "<-"
        tknRightArrow = (Arrow R <$) <$> lexStr "->"

tknString :: Lexer (Atom Tkn)
tknString = fmap (StringLiteral <$>)
    (lexChar '\"' *>
        lexSpan (not <$> (== '\"'))
    <* lexChar '\"')

tknOpenParen :: Lexer (Atom Tkn)
tknOpenParen = (OpenParen <$) <$> lexChar '('

tknCloseParen :: Lexer (Atom Tkn)
tknCloseParen = (CloseParen <$) <$> lexChar ')'

tknOpenBrace :: Lexer (Atom Tkn)
tknOpenBrace = (OpenBrace <$) <$> lexChar '{'

tknCloseBrace :: Lexer (Atom Tkn)
tknCloseBrace = (CloseBrace <$) <$> lexChar '}'

tknWord :: Lexer (Atom Tkn)
tknWord = fmap (Word <$>) (lexSpan $ (&&) <$>
        (not <$> isSpace) <*>
        (not <$> flip elem specialChars)
    )

lexTkn :: Lexer (Atom Tkn)
lexTkn = tknKwrdFor   <|> tknKwrdIn     <|> tknKwrdCase  <|> tknKwrdLet    <|>
         tknOpenParen <|> tknCloseParen <|> tknOpenBrace <|> tknCloseBrace <|>
         tknArrow     <|> tknString     <|> tknWord

lex :: [Atom Char] -> [Atom Tkn] -> Maybe [Atom Tkn]
lex [] tkns = Just tkns
lex input tkns = do
    (input', tkn) <- runLexer (ws *> lexTkn <* ws) input
    tkns' <- Lexer.lex input' (tkns ++ [tkn])
    if null input' then do
        let
            (Atom _ fileName (_, col)) = tkn
            eof = Atom EndOfFile fileName (1, col + 1)
        return $ tkns' ++ [eof]
    else do
        return tkns'