// authored by ln220
module AssertionTests

open AssertionTypes 
open AssertionCheck 
open AssertionEvaluation
open SimulatorTypes 
open CommonTypes 
open FastCreate

let rec print(tree: ExprInfo) : string=
    match tree with
    | Lit lit, pos->

        match lit with
        | Value value ->

            match value with
            | Int int -> string int  
            | Uint uint -> string uint
            | Bool bool -> string bool  

        | Id (id, _, _) -> id

    | Cast c, pos ->

        match c with
        | ToSigned e -> "int(" + (print e) + ")"
        | ToUnsigned e -> "uint" + (print e) + ")" 
        | ToBool e -> "bool" + (print e) + ")" 

    | Add (BinOp(l, r)), pos -> "(" + print l + "+" + (print r) + ")"

    | Sub(BinOp(l, r)), pos -> "(" + print l + "-" + (print r) + ")"

    | Mul(BinOp(l, r)), pos -> "(" + print l + "*" + (print r) + ")"

    | Div(BinOp(l, r)), pos -> "(" + print l + "/" + (print r) + ")"

    | Rem(BinOp(l, r)), pos -> "(" + print l + "%" + (print r) + ")"

    | BitAnd(BinOp(l, r)), pos -> "(" + print l + "&&&" + (print r) + ")"

    | BitOr(BinOp(l, r)), pos -> "(" + print l + "|||" + (print r) + ")"

    | BitNot(UnOp(op)), pos -> "(~~~"+ (print op) + ")"

    | BoolExpr (boolExpr), pos-> 
        match boolExpr with
        | Neq(BinOp(l, r)) -> "(" + (print l) + "<>" + (print r) + ")"
        | Eq(BinOp(l, r)) -> "(" + (print l) + "==" + (print r) + ")"
        | LogAnd(BinOp(l, r)) -> "(" + (print l) + "&&" + (print r) + ")"
        | LogOr(BinOp(l, r)) -> "(" + (print l) + "||" + (print r) + ")"
        | Lt(BinOp(l, r)) -> "(" + (print l) + "<" + (print r) + ")"
        | Gt(BinOp(l, r)) -> "(" + (print l) + ">" + (print r) + ")"
        | Gte(BinOp(l, r)) -> "(" + (print l) + ">=" + (print r) + ")"
        | Lte(BinOp(l, r)) -> "(" + (print l) + "<=" + (print r) + ")"
        | LogNot(UnOp op) -> "(not"+ (print op) + ")"
        | _ -> failwithf "not implemented yet"

    | _ -> string 0


let fsList comp = 
    createFastComponent 5 comp []// applied the curried function to the last argument

let makeGhostComp id label cType = 
    {Id = ComponentId id;
    Type = cType; 
    Label = ComponentLabel label;
    Inputs =  Map [];
    Outputs = Map [(OutputPortNumber 0, [ComponentId "ciao", InputPortNumber 0])]; 
    CustomSimulationGraph = None; 
    State = NoState}

let makeComp id label cType = 
    {
        Id = id;
        Type = cType; 
        Label = label; 
        InputPorts = []; 
        OutputPorts = []; 
        X = 1.;
        Y = 1.; 
        H = 1.; 
        W = 1.; 
        SymbolInfo = None
    }

/// test compilation of AST: size error
let testCheck1 () : Result<string, string>= 
    // ((3+2)>5)<true
    let lit3 = Lit(Value(Int(3))), {Line = 0; Col = 10; Length = 1}
    let lit2 = Lit(Value(Int(2))), {Line = 0; Col = 8; Length = 1}
    let lit5 = Lit(Value(Int(5))), {Line = 0; Col = 13; Length = 1}
    let addExpr = Add(BinOp(lit3, lit2)), {Line = 0; Col = 7; Length = 5}
    let gtExpr = BoolExpr(Gt(BinOp(addExpr, lit5))), {Line = 0; Col = 7; Length = 8}
    let litTrue = Lit(Value(Bool true)), {Line = 0; Col = 1; Length= 5}
    
    let tree = BoolExpr(Lt(BinOp(litTrue, gtExpr))), {Line = 0; Col = 0; Length = 16} 
    let compile = checkAST tree []
    match compile with 
    | ErrLst eLst -> 
        match eLst with 
        | e::_ -> 
            if (e = {Msg = "The buses have different widths. Left expr is of size: 3. Right expr is of size: 4"; Pos = {Line = 0; Col = 7; Length = 8}})
            then Ok($"The following expr: {print tree}, should not compile because {e.Msg}")
            else Error($"wrong error returned for the following expr: {print tree}: {e}")
        | lst -> Error($"too many errors: {lst}")
    | _ -> Error("The following expression: {print tree} should not compile. It should give a width error")
    

/// test compilation of AST: width checking of bus
let testCheck2 (): Result<string, string> = 
    // true < ((3+4)>5)
    let lit3u = Lit(Value(Uint(3u))), {Line = 0; Col = 10; Length = 1}
    let litA = Lit(Id("a", 0, "")), {Line = 0; Col = 8; Length = 1}
    let lit5u = Lit(Value(Uint(5u))), {Line = 0; Col = 13; Length = 1}
    let addExpr = Add(BinOp(lit3u, litA)), {Line = 0; Col = 7; Length = 5}
    let gtExpr = BoolExpr(Gt(BinOp(addExpr, lit5u))), {Line = 0; Col = 7; Length = 8}
    let litTrue = Lit(Value(Bool true)), {Line = 0; Col = 1; Length= 5}
    let comp = makeComp "compA" "a" (Viewer 3)
    let tree = BoolExpr(Lt(BinOp(litTrue, gtExpr))), {Line = 0; Col = 0; Length = 16} 
    let compile = checkAST tree [comp]
    match compile with
    | TypeInfo t  when t = BoolType -> Ok($"The expr: {print tree} was successfully compiled. It has type {t}")
    | TypeInfo  t -> Error($"Wrong evaluation of the following expression: {print tree}. It was expected to have type Bool and size 1, but it has type {t} instead ")
    | ErrLst e -> Error($"the compilation of {print tree} failed and it wrongly detected the following errors: {e}")  


let testCheck3 (): Result<string, string> = 
    // (a + b) == 0 (with a and b being of different widths)
    let lit0u = Lit(Value(Uint(0u))), {Line = 0; Col = 10; Length = 1}
    let litA = Lit(Id("a", 0, "")), {Line = 0; Col = 1; Length = 1}
    let litB = Lit(Id("b", 0, "")), {Line = 0; Col = 3; Length = 1}
    let addExpr = Add(BinOp(litA, litB)), {Line = 0; Col = 7; Length = 5}
    let eqExpr = BoolExpr(Eq(BinOp(addExpr, lit0u))), {Line = 0; Col = 7; Length = 8}

    let compA = makeComp "compA" "a" (Viewer 3)
    let compB = makeComp "compB" "b" (Viewer 5)

    let compile = checkAST eqExpr [compA; compB]
    match compile with
    | TypeInfo  t-> Error($"the expr {print eqExpr} should not compile as a and b are of different widths") 
    | ErrLst e -> 
        if e.Head = {Msg = "The buses have different widths. Left expr is of size: 3. Right expr is of size: 5"; Pos = {Line = 0; Col = 7; Length = 5}} 
        then Ok($"the expression: {print eqExpr} does not compile as it is trying to add buses of different widths")
        else Error($"wrong error message {e} for the expression {print eqExpr}")

let testCheck4 (): Result<string, string> = 
    // (a + b) == true (with a and b being of different widths)
    let litTrue = Lit(Value(Bool(true))), {Line = 0; Col = 10; Length = 1}
    let litA = Lit(Id("a", 0, "")), {Line = 0; Col = 1; Length = 1}
    let litB = Lit(Id("b", 0, "")), {Line = 0; Col = 3; Length = 1}
    let mulExpr = Mul(BinOp(litA, litB)), {Line = 0; Col = 7; Length = 5}
    let eqExpr = BoolExpr(Eq(BinOp(mulExpr, litTrue))), {Line = 0; Col = 7; Length = 8}

    let compA = makeComp "compA" "a" (Viewer 5)
    let compB = makeComp "compB" "b" (Viewer 5)

    let compile = checkAST eqExpr [compA; compB]
    match compile with
    |  TypeInfo t -> Error($"the expr {print eqExpr} should not compile as a and b are of different widths") 
    | ErrLst e -> 
        if e.Head = {Msg = "This function can't be applied on value of different types. left expr is of type: UintType. Right expr is of type: BoolType"; Pos = {Line = 0; Col = 7; Length = 8}} 
        then Ok($"the expression: {print eqExpr} does not compile as it is trying to add buses of different widths")
        else Error($"wrong error message {e} for the expression {print eqExpr}")

