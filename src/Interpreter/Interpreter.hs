module Interpreter where {
    import AST (Tape (..), Pat, Rule (..), Direction (..), AST (..));
    import Text.Printf (printf);
    import Control.Monad (when);
    import Data.List (find);

    newtype InterpreterSettings = InterpreterSettings {
        interpreterSettingsDebug :: Bool
    };

    interpretAST :: InterpreterSettings -> AST -> IO ();
    interpretAST settings (AST (tape:tapes) rules) = do {
        _ <- interpretTape settings tape rules;
        interpretAST settings (AST tapes rules);
    };
    interpretAST _ (AST [] _) = return ();

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

    showValues :: [Pat] -> String;
    showValues (x:xs) = " " ++ show x ++ showValues xs;
    showValues [] = "";

    type Offset = Int;
    type Length = Int;
    type Index = Int;
    idxOffsetLen :: [Pat] -> Index -> Offset -> Maybe (Offset, Length);
    idxOffsetLen (x:_) 0 offset = Just (offset, length $ show x);
    idxOffsetLen (x: xs) idx offset
        | idx < 0   = Nothing
        | otherwise = idxOffsetLen xs (idx - 1) (offset + length (show x) + 1);
    idxOffsetLen [] _ _ = Nothing;

    applyRule :: Tape -> Rule -> Tape;
    applyRule tape rule = Tape tName rNextState newTValues newTIdx where {
        (Tape tName _ tValues tIdx) = tape;
        (Rule _ _ rToValue rDir rNextState) = rule;
        newTValues = take tIdx tValues ++ rToValue : drop (tIdx + 1) tValues;
        newTIdx = case rDir of {
            L -> tIdx - 1;
            R -> tIdx + 1;
        };
    };

    canApplyRule :: Tape -> Rule -> Bool;
    canApplyRule tape rule = case tapeValueAtIdx of {
        Just tValue -> tState == rCurrentState && tValue == rFromValue;
        Nothing -> False;
    } where {
        (Tape _ tState tValues tIdx) = tape;
        (Rule rCurrentState rFromValue _ _ _) = rule;
        tapeValueAtIdx = tValues !? tIdx;

        infixl 9 !?;
        (!?) :: [a] -> Int -> Maybe a;
        []     !? _     = Nothing;
        (x:_)  !? 0     = Just x;
        (_:xs) !? idx
            | idx > 0   = xs !? (idx - 1)
            | otherwise = Nothing;
    }
}