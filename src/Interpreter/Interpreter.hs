module Interpreter where {
    import AST (AST (..));
    import Text.Printf (printf);
    import Control.Monad (when);
    import Data.List (find);
    import Rule (Rule (..), applyRule, canApplyRule);
    import Pattern (Pat);
    import Tape (Tape(..));
    
    newtype InterpreterSettings = InterpreterSettings {
        interpreterSettingsDebug :: Bool
    };

    interpretAST :: InterpreterSettings -> AST -> IO ();
    interpretAST settings (AST sets (tape:tapes) rules) = do {
        _ <- interpretTape settings tape rules;
        interpretAST settings (AST sets tapes rules);
    };
    interpretAST _ (AST _ [] _) = return ();

    interpretTape :: InterpreterSettings -> Tape -> [Rule] -> IO ();
    interpretTape settings tape rules = do {
        when (interpreterSettingsDebug settings) $
            printTape tape;
        let {
            rule = find (canApplyRule tape) rules;
        };
        case applyRule tape <$> rule of {
            Just newTape -> interpretTape settings newTape rules;
            Nothing -> return ();
        };
    };

    printTape :: Tape -> IO ();
    printTape (Tape _ state values idx) = do {
        _ <- printf "%s:" (show state);
        _ <- putStrLn $ showValues values;
        _ <- case idxOffsetLen values idx 0 of {
            Just (offset, len) -> putStr $
                replicate (length (show state) + 2 + offset) ' ' ++
                take len ("^" ++ repeat '~');
            Nothing -> ioError $ userError $ printf
                "tried to access value of index %d when there's only %d number of patters"
                idx
                (length values);
        };
        _ <- putStrLn "";
        return ();
    };

    showValues :: [Pat String] -> String;
    showValues (x:xs) = " " ++ show x ++ showValues xs;
    showValues [] = "";

    type Offset = Int;
    type Length = Int;
    type Index = Int;
    idxOffsetLen :: [Pat String] -> Index -> Offset -> Maybe (Offset, Length);
    idxOffsetLen (x:_) 0 offset = Just (offset, length $ show x);
    idxOffsetLen (x: xs) idx offset
        | idx < 0   = Nothing
        | otherwise = idxOffsetLen xs (idx - 1) (offset + length (show x) + 1);
    idxOffsetLen [] _ _ = Nothing;
}