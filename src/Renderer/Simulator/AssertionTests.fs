module AssertionTests

open AssertionTypes 
open AssertionCheck 
open SimulatorTypes 
open CommonTypes 
open FastCreate

let fsList comp = 
    createFastComponent 5 comp // applied the curried function to the last argument

let makeGhostComp id label cType = 
    {Id = ComponentId id;
    Type = cType; 
    Label = ComponentLabel label;
    Inputs =  Map [];
    Outputs = Map [(OutputPortNumber 0, [ComponentId "ciao", InputPortNumber 0])]; 
    CustomSimulationGraph = None; 
    State = NoState}

let test1 () = 
    // ((3+5)>5)<true
    let lit3 = Lit(Value(Int(3))), {Line = 0; Col = 10; Length = 1}
    let lit4 = Lit(Value(Int(4))), {Line = 0; Col = 8; Length = 1}
    let lit5 = Lit(Value(Int(5))), {Line = 0; Col = 13; Length = 1}
    let addExpr = Add(BinOp(lit3, lit4)), {Line = 0; Col = 7; Length = 5}
    let gtExpr = BoolExpr(Gt(BinOp(addExpr, lit5))), {Line = 0; Col = 7; Length = 8}
    let litTrue = Lit(Value(Bool true)), {Line = 0; Col = 1; Length= 5}
    
    let tree = BoolExpr(Lt(BinOp(litTrue, gtExpr))), {Line = 0; Col = 0; Length = 16} 
    let compile = checkAST tree []
    match compile with 
    | ErrLst eLst -> 
        match eLst with 
        | e::_ -> 
            if (e = {Msg = "The buses have different widths. Left expr is of size: 3. Right expr is of size: 4"; Pos = {Line = 0; Col = 7; Length = 8}})
            then printf "the expression was correctly determined as wrong. "
            else printf "wrong error returned: %A" e
        | _ -> printf "too many errors"
    | _ -> printf "wrong evaluation of the ast"
    1
