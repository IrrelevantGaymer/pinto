module Sets where {
    import Pattern (Pat);
    
    type UnOp = UnarySetOperation;
    data UnarySetOperation = PowerSet deriving(Show);
    type BinOp = BinarySetOperation;
    data BinarySetOperation = Concat | Difference | CartesianProduct deriving(Show);

    data SetDef = Set [Pat] | 
        UnOp UnOp SetDef |
        BinOpSet BinOp SetDef SetDef deriving(Show);
}