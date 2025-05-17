module Main where 

import System.Environment

import Lexer

main :: IO()
main = do
    args <- getArgs
    srcs <- sequenceA $ readFile <$> args
    tkns <- case Lexer.lex (tokenize args srcs) [] of
        Just tkns -> return tkns
        Nothing -> ioError $ userError "Could Not Tokenize Files"
    print tkns

tokenize :: [FileName] -> [String] -> [Atom Char]
tokenize filePaths srcs = concat $ map 
    (uncurry (flip atomize (1, 1))) 
    (zip filePaths srcs)
