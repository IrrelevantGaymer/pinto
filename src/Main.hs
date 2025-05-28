module Main where {
    import System.Environment ( getArgs );

    import Lexer ( atomize, lex, FileName, Atom );
    import Tokens ( Tkn );
    import Parser (parse);
    import Interpreter (InterpreterSettings (..), interpretAST);
    import AST (AST(..), emptyAST);;

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
            ast <- case parse tkns $ AST [] [] of {
                Right ast -> return ast;
                Left err -> ioError $ userError $ show err;
            };
            print ast;
        };
        "interpret" -> do {
            tkns <- tokenize rest;
            ast <- case parse tkns emptyAST of {
                Right ast -> return ast;
                Left err -> ioError $ userError $ show err;
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