module Sets where {
    import Pattern (Pat);
    
    type UnOp = UnarySetOperation;
    data UnarySetOperation = PowerSet deriving(Show);
    type BinOp = BinarySetOperation;
    data BinarySetOperation = Union | Difference | CartesianProduct deriving(Show);

    type Precedence = Int;
    getPrecedence :: BinOp -> Precedence;
    getPrecedence CartesianProduct = 1;
    getPrecedence Union            = 2;
    getPrecedence Difference       = 2;

    data SetDef = Set ![Pat] | 
        UnOpSet !UnOp !SetDef |
        BinOpSet !BinOp !SetDef !SetDef |
        Id !SetDef deriving(Show);
}