module AST where

data AST = AST {
    astStartingTapes :: [Tape],
    astRules :: [Rule]
} deriving(Show)

data Node = RuleNode Rule | TapeNode Tape

type UnOp = UnarySetOperation
data UnarySetOperation = PowerSet deriving(Show)
type BinOp = BinarySetOperation
data BinarySetOperation = Concat | Difference | CartesianProduct deriving(Show)

data SetDef = Set [Pat] | 
    UnOp UnOp SetDef |
    BinOpSet BinOp SetDef SetDef deriving(Show) 

type Dir = Direction
{-
 - In consideration: Implement a Stay Direction.
 - Technically pure Turing Machines can only move
 - once left or right after applying a rule,
 - however it is trivial to define a rule which
 - encodes a stay operation.
 - ```
 - case SomeState a b -> (Reset NextState)
 - case (Reset a) _ _ <- a
 - ```
 -
 - That same logic also applies to an arbitrary
 - number of steps in a specific direction or
 - a specific location on the tape.  However this logic 
 - is a bit more difficult to encode.  For the following
 - example, assume we have a defined Int set built from 
 - S expressions/Tuples and a Dec state and `!` is the
 - Stay Direction
 - ```
 - case SomeState a b ! (Right n NextState)
 - for a n NextState in Any * Int * Any case (Right n NextState) a n ! (Dec (ApplyRight a NextState))
 - for a n NextState in Any * Int * Any case (ApplyRight a NextState) n a -> (Right n NextState)
 - ```
 - 
 - Since there exists sets and transformation rules whose behavior
 - is isomorphic to a Stay direction and Arbitrary Move directions,
 - adding these would stay create "vanilla" Turing Machines.
 - However, I like the actual feel of "true" Turing Machines,
 - so I might hide these features behind a feature flag OR
 - a macro system
 -}
data Direction = L | R deriving(Show)

type Pat = Pattern
data Pattern = Value String | List [Pattern] | Tuple [Pattern] deriving(Show)

data Rule = Rule {
    ruleCurrentState :: Pat,
    ruleFromValue :: Pat,
    ruleToValue :: Pat,
    ruleDir :: Dir,
    ruleNextState :: Pat
} deriving(Show)

data Tape = Tape {
    tapeName :: String,
    tapeState :: Pat,
    tapeValues :: [Pat], -- TODO: Create Data Structure for storing the Tape Values
    -- Maybe use an MArray or MVector because we will be mutating specific indices
    -- or maybe if theres a MVecDeque data type so we can easily prepend elements as well
    tapeIndex :: Int
} deriving(Show)

data Map = Map {
    mapName :: String,
    mapFromSet :: SetDef,
    mapToSet :: SetDef,
    mapMappings :: [Mapping]
} deriving(Show)

data Mapping = Mapping {
    mappingFromPattern :: Pat,
    mappingToPattern :: Pat
} deriving(Show)