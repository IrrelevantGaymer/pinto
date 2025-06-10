module Main where {
    import System.Environment ( getArgs );

    import Lexer ( atomize, lex, FileName, Atom );
    import Tokens ( Tkn );
    import Parser (parse);
    import Interpreter (InterpreterSettings (..), interpretAST);
    import FullResult (FullResult(..));
    
    main :: IO();
    main = do {
        args <- getArgs;
        handleArgs args;
    };


    handleArgs :: [String] -> IO ();
    handleArgs (cmd:rest) = case cmd of {
        "lex" -> do {
            tkns <- tokenize rest;
            debugPrintTokens tkns;
        }; 
        "parse" -> do {
            tkns <- tokenize rest;
            ast <- case parse tkns mempty of {
                Ok ast -> return ast;
                Soft err -> ioError $ userError $ show err;
                Hard err -> ioError $ userError $ show err;
                Empty -> ioError $ userError "parsing resulted in empty";
            };
            print ast;
        };
        "interpret" -> do {
            tkns <- tokenize rest;
            ast <- case parse tkns mempty of {
                Ok ast -> return ast;
                Soft err -> ioError $ userError $ show err;
                Hard err -> ioError $ userError $ show err;
                Empty -> ioError $ userError "parsing resulted in empty";
            };
            interpretAST (InterpreterSettings True) ast;
        };
        "help" -> putStrLn "TODO: provide help function";
        (_:_) -> putStrLn $ cmd ++ " is not a valid command";
        [] -> putStrLn "unreachable";
    };
    handleArgs [] = putStrLn "Expected command.  Try help";

    tokenize :: [FileName] -> IO [Atom Tkn];
    tokenize filePaths = do {
        srcs <- sequenceA $ readFile <$> filePaths;
        case Lexer.lex (atomize filePaths srcs) [] of {
            Just tkns -> return tkns;
            Nothing -> ioError $ userError "Could Not Tokenize Files";
        };
    };

    debugPrintTokens :: [Atom Tkn] -> IO();
    debugPrintTokens (tkn:tkns) = do {
        _ <- print tkn;
        debugPrintTokens tkns;
    };
    debugPrintTokens [] = putStrLn "";
}