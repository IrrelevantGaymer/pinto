module Tape where {
    import Pattern (Pat);

    data Tape = Tape {
        tapeName :: String,
        tapeState :: Pat String,
        tapeValues :: [Pat String], -- TODO: Create Data Structure for storing the Tape Values
        -- Maybe use an MArray or MVector because we will be mutating specific indices
        -- or maybe if theres a MVecDeque data type so we can easily prepend elements as well
        tapeIndex :: Int
    } deriving(Show);
}