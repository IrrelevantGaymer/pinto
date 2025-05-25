module Main where

import System.Environment ( getArgs )

import Lexer ( atomize, lex, FileName, Atom )
import Tokens ( Tkn )

main :: IO()
main = do
    args <- getArgs
    handleArgs args


handleArgs :: [String] -> IO ()
handleArgs (cmd:rest) = (\args -> case cmd of
    "lex" -> do
        tkns <- tokenize args
        debugPrintTokens tkns
    "parse" -> putStrLn "Parsing is not supported yet"
    "interpret" -> putStrLn "Interpreting is not supported yet"
    "help" -> putStrLn "TODO: provide help function"
    (_:_) -> putStrLn $ cmd ++ " is not a valid command"
    [] -> putStrLn "unreachable") rest
handleArgs [] = putStrLn "Expected command.  Try help"

tokenize :: [FileName] -> IO [Atom Tkn]
tokenize filePaths = do
    srcs <- sequenceA $ readFile <$> filePaths
    (\paths -> case Lexer.lex (atomize paths srcs) [] of
        Just tkns -> return tkns
        Nothing -> ioError $ userError "Could Not Tokenize Files") filePaths

debugPrintTokens :: [Atom Tkn] -> IO()
debugPrintTokens (tkn:tkns) = do
    _ <- print tkn
    debugPrintTokens tkns
debugPrintTokens [] = putStrLn ""