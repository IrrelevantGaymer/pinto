{-# OPTIONS_GHC -Wno-x-partial #-}
module Parser.Rule.BuiltInRule where {
    import Parser.Tape (Tape (..));
    import Parser.Pattern (Pattern(..));
    import Data.Char (isSpace);
    import Data.List (intersperse);
    
    {-
     - Print = for (a b) in All * All case (Print a) b b -> (Reset a)
     - Read  = for (a b c) in All * All * All case (Read  a) b c -> (Reset a)
     -       note: Read is weird/special because it gets the new tape value
     -             from the outside world
     - GetHeadAddr = for (a b c) in All * All * Int case (GetHeadAddr a) b c -> (Reset a) 
     -}
    data BuiltInRule = Show | ShowLn | Print | PrintLn | ReadChar | ReadString | ReadLine | GetHeadAddr | PrintTape;

    builtInRules :: [BuiltInRule];
    builtInRules = [
        Show, ShowLn, Print, PrintLn, 
        ReadChar, ReadString, ReadLine, 
        GetHeadAddr,
        PrintTape -- PrintTape should be temporary because we should be able to build this once we have the complete feature set
    ];

    applyBuiltInRule :: Tape -> BuiltInRule -> IO Tape;
    applyBuiltInRule tape Show 
        | Tuple [Value "Show", a] <- tState = do {
            _ <- putStr $ show $ tValues !! tIdx;
            return $ Tape tName (Tuple [Value "Reset", a]) tValues (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule Show"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };
    applyBuiltInRule tape ShowLn 
        | Tuple [Value "ShowLn", a] <- tState = do {
            _ <- print $ tValues !! tIdx;
            return $ Tape tName (Tuple [Value "Reset", a]) tValues (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule ShowLn"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };
    applyBuiltInRule tape Print 
        | Tuple [Value "Print", a] <- tState, 
          Value s <- tValue,
          length s > 1,
          head s == '\"',
          last s == '\"' 
        = do {
            let {
                string = init $ tail s;
                tValues' = take tIdx tValues ++ Value string : drop (tIdx + 1) tValues;
            };
            _ <- putStr string;
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule Print"
    where {
        (Tape tName tState tValues tIdx) = tape;
        tValue = tValues !! tIdx;
    };
    applyBuiltInRule tape PrintLn
        | Tuple [Value "PrintLn", a] <- tState, 
          Value s <- tValue,
          length s > 1,
          head s == '\"',
          last s == '\"' 
        = do {
            let {
                string = init $ tail s;
                tValues' = take tIdx tValues ++ Value string : drop (tIdx + 1) tValues;
            };
            _ <- putStrLn string;
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule PrintLn"
    where {
        (Tape tName tState tValues tIdx) = tape;
        tValue = tValues !! tIdx;
    };
    applyBuiltInRule tape ReadChar
        | Tuple [Value "ReadChar", a] <- tState = do {
            char <- getChar;
            let {
                tValues' = take tIdx tValues ++ Value [char] : drop (tIdx + 1) tValues;
            };
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule ReadChar"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };
    applyBuiltInRule tape ReadString
        | Tuple [Value "ReadString", a] <- tState = do {
            string <- getString;
            let {
                string' = "\"" ++ string ++ "\"";
                tValues' = take tIdx tValues ++ Value string' : drop (tIdx + 1) tValues;
            };
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule ReadString"
    where {
        (Tape tName tState tValues tIdx) = tape;
        getString = do {
            char <- getChar;
            if not $ isSpace char then 
                (char:) <$> getString 
            else 
                return "";
        };
    };
    applyBuiltInRule tape ReadLine
        | Tuple [Value "ReadLine", a] <- tState = do {
            line <- getLine;
            let {
                line' = "\"" ++ line ++ "\"";
                tValues' = take tIdx tValues ++ Value line' : drop (tIdx + 1) tValues;
            };
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule ReadLine"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };
    applyBuiltInRule tape GetHeadAddr
        | Tuple [Value "GetHeadAddr", a] <- tState = do {
            let {
                tValues' = take tIdx tValues ++ Num tIdx : drop (tIdx + 1) tValues;
            };
            return $ Tape tName (Tuple [Value "Reset", a]) tValues' (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule GetHeadAddr"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };
    applyBuiltInRule tape PrintTape
        | Tuple [Value "PrintTape", Num start, Num end, a] <- tState = do {
            let {
                tapeToPrint = take (end - start + 1) $ drop start tValues;
            };
            _ <- putStrLn $ intersperse ' ' $ concatMap show tapeToPrint;

            return $ Tape tName (Tuple [Value "Reset", a]) tValues (tIdx + 1);
        }
        | otherwise = ioError $ error "Could not apply built in rule GetHeadAddr"
    where {
        (Tape tName tState tValues tIdx) = tape;
    };

    canApplyBuiltInRule :: Tape -> BuiltInRule -> Bool;
    canApplyBuiltInRule (Tape _ tState _ _) Show 
        | Tuple [Value "Show", _] <- tState = True
        | otherwise                          = False;
    canApplyBuiltInRule (Tape _ tState _ _) ShowLn 
        | Tuple [Value "ShowLn", _] <- tState = True
        | otherwise                          = False;
    canApplyBuiltInRule (Tape _ tState tValues tIdx) Print
        | Tuple [Value "Print", _] <- tState, 
          Value s <- tValues !! tIdx,
          length s > 1,
          head s == '\"',
          last s == '\"' = True
        | otherwise = False;
    canApplyBuiltInRule (Tape _ tState tValues tIdx) PrintLn
        | Tuple [Value "PrintLn", _] <- tState, 
          Value s <- tValues !! tIdx,
          length s > 1,
          head s == '\"',
          last s == '\"' = True
        | otherwise = False;
    canApplyBuiltInRule (Tape _ tState _ _) ReadChar
        | Tuple [Value "ReadChar", _] <- tState = True
        | otherwise                         = False;
    canApplyBuiltInRule (Tape _ tState _ _) ReadString
        | Tuple [Value "ReadString", _] <- tState = True
        | otherwise                         = False;
    canApplyBuiltInRule (Tape _ tState _ _) ReadLine
        | Tuple [Value "ReadLine", _] <- tState = True
        | otherwise                         = False;
    canApplyBuiltInRule (Tape _ tState _ _) GetHeadAddr
        | Tuple [Value "GetHeadAddr", _] <- tState = True
        | otherwise                                = False;
    canApplyBuiltInRule (Tape _ tState _ _) PrintTape
        | Tuple [Value "PrintTape", Num _, Num _, _] <- tState = True
        | otherwise                                            = False;
}
