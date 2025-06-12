module Main where {
    import System.Environment ( getArgs );

    import Lexer.Lexer ( atomize, FileName, Atom );
    import Lexer.Tokens ( Tkn );
    import Parser.Parser (parse);
    import Interpreter.Interpreter (InterpreterSettings (..), interpretAST);
    import FullResult (FullResult(..));
    import qualified Lexer.Lexer as Lexer;
    
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
            interpretAST (InterpreterSettings False) ast;
        };
        "dinterpret" -> do {
            tkns <- tokenize rest;
            ast <- case parse tkns mempty of {
                Ok ast -> return ast;
                Soft err -> ioError $ userError $ show err;
                Hard err -> ioError $ userError $ show err;
                Empty -> ioError $ userError "parsing resulted in empty";
            };
            interpretAST (InterpreterSettings True) ast;
        };
        "help" -> putStrLn 
            "help       - provides descriptions and instructions on pinto commands\n\
            \lex        [filepaths] - tokenizes the provided files and prints them out for debug purposes\n\
            \parse      [filepaths] - parses the provided files and prints out the programs' sets, rules, and tapes for debug purposes\n\
            \interpret  [filepaths] - interprets the provided files as a program\n\
            \dinterpret [filepaths] - interprets the provided files as a program, printing out the machine's state, head, and tape at every step\
            \";
        (_:_) -> putStrLn $ cmd ++ " is not a valid command.  Try help";
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