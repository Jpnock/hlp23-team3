module AssertionTests

open AssertionTypes 
open AssertionCheck 
open AssertionEvaluation
open SimulatorTypes 
open CommonTypes 
open FastCreate

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
            then Ok($"The following expr: should not compile because {e.Msg}")
            else Error($"wrong error returned: {e}")
        | lst -> Error($"too many errors: {lst}")
    | _ -> Error("The following expression should not compile. It should give a width error")
    

/// test compilation of AST: width checking of bus
let testCheck2 (): Result<string, string> = 
    // true < ((3+4)>5)
    let lit3u = Lit(Value(Uint(3u))), {Line = 0; Col = 10; Length = 1}
    let litA = Lit(Id("a")), {Line = 0; Col = 8; Length = 1}
    let lit5u = Lit(Value(Uint(5u))), {Line = 0; Col = 13; Length = 1}
    let addExpr = Add(BinOp(lit3u, litA)), {Line = 0; Col = 7; Length = 5}
    let gtExpr = BoolExpr(Gt(BinOp(addExpr, lit5u))), {Line = 0; Col = 7; Length = 8}
    let litTrue = Lit(Value(Bool true)), {Line = 0; Col = 1; Length= 5}
    let comp = 
        makeGhostComp "compA" "a" (Viewer 3)
        |> fsList
    let tree = BoolExpr(Lt(BinOp(litTrue, gtExpr))), {Line = 0; Col = 0; Length = 16} 
    let compile = checkAST tree [comp]
    match compile with
    | Properties {Type = t; Size = s} when t = BoolType && s = 1 -> Ok($"The AST was successfully compiled. It has type {t} and size {s}")
    | Properties {Type = t; Size = s} -> Error($"Wrong evaluation of the following expression. It was expected to have type Bool and size 1, but it has type {t} and size {s} instead ")
    | ErrLst e -> Error($"the compilation failed and it wrongly detected the following errors: {e}")  


let makeSimpleFs fcArray assertions fComps: FastSimulation = 
    let sg = Map []
    let gatherData = {
        Simulation = sg; 
        CustomInputCompLinks = Map []
        CustomOutputCompLinks = Map []; 
        CustomOutputLookup = Map []; 
        Labels = Map []; 
        AllComps = Map [];
    }
    {
        ClockTick = 5; 
        MaxStepNum = 10; 
        MaxArraySize = 10; 
        FGlobalInputComps = Array.empty;
        FConstantComps = fcArray;
        FClockedComps = Array.empty; 
        FOrderedComps = fcArray; 
        FIOActive = Map []; 
        FIOLinks = []; 
        FComps = Option.defaultValue (Map []) fComps; 
        FCustomComps = Map []; 
        WaveComps = Map []; 
        FSComps = Map []; 
        FCustomOutputCompLookup = Map []; 
        G = gatherData; 
        NumStepArrays = 10; 
        Drivers = Array.empty; 
        WaveIndex = Array.empty; 
        ConnectionsByPort = Map []; 
        ComponentsById = Map []; 
        SimulatedCanvasState = []; 
        SimulatedTopSheet = "this"; 
        Assertions = assertions;
    }

let makeFastComponent id label width startS endS values sc: FastComponent= 
    let beginning = if startS = 0u then [] else [0u..startS - 1u]
    let extSteps = beginning @ [startS..endS]
    let extValues  = 
        values @ (beginning|> List.map (fun el -> 0u))
    let outputs =
        extSteps
        |> List.zip extValues
        |> List.map (fun (step, value) -> Data {Dat = Word value; Width = width}) 
        |> List.toArray
        |> makeStepArray

    {
        fId = ComponentId id, []; 
        cId = ComponentId id; 
        FType = Viewer width; 
        State = None; 
        Active = true; 
        OutputWidth = Array.empty; 
        InputLinks = Array.empty; 
        InputDrivers = Array.empty; 
        Outputs = [|outputs|] ; 
        SimComponent = sc; 
        AccessPath = [];
        FullName = "component"; 
        FLabel = label; 
        SheetName = ["this"]; 
        Touched = true; 
        DrivenComponents = [];
        NumMissingInputValues = 0; 
        VerilogOutputName = Array.empty; 
        VerilogComponentName = "";

    }
/// test evaluation of AST: basic evaluation, no buses (irrealistic)
let testEvaluate1(): Result<string, string> = 
    // (1+5)<2 -> false 
    let lit1 = Lit(Value(Int(1))), {Line = 0; Col = 1; Length = 1}
    let lit5 = Lit(Value(Int(5))), {Line = 0; Col = 3; Length = 1}
    let lit2 = Lit(Value(Int(2))), {Line = 0; Col = 5; Length = 1}
    let addExpr = Add(BinOp(lit1, lit5)), {Line = 0; Col = 0; Length = 5}
    let gtExpr = BoolExpr(Lt(BinOp(addExpr, lit2))), {Line = 0; Col = 0; Length = 8}
    let fs = makeSimpleFs Array.empty [] None
    let check = checkAST gtExpr (Array.toList fs.FConstantComps)
    match check with 
    | Properties p -> 
        let res = evaluate gtExpr fs 0
        match res with 
        | Bool b, Size s when b = false && s = 1 -> Ok("the AST was compiled and evaluated correctly")
        | Bool b, Size s -> Error($"returned the wrong result. {b} {s}")
        | t, Size s -> Error($"returned the wrong result type {t}")
    | ErrLst e -> Error($"the AST did not compile. There were the following errors: {e}")

/// test evaluation of AST: basic evaluation and result retrieval from fs
let testEvaluate2(): Result<string, string> = 
    //(1+5) < a -> false (a = 2)
    let lit1 = Lit(Value(Int(1))), {Line = 0; Col = 1; Length = 1}
    let lit5 = Lit(Value(Int(5))), {Line = 0; Col = 3; Length = 1}
    let litA = Lit(Id("a")), {Line = 0; Col = 5; Length = 1}
    let addExpr = Add(BinOp(lit1, lit5)), {Line = 0; Col = 0; Length = 5}
    let gtExpr = BoolExpr(Lt(BinOp(addExpr, litA))), {Line = 0; Col = 0; Length = 8}
    let simComp = makeGhostComp "compA" "a" (Viewer 5) 
    let fastComp = makeFastComponent "compA" "a" 5 0u 0u [2u] simComp
    let fs = makeSimpleFs Array.empty [] (Some(Map[((ComponentId "a", []), fastComp)]))
    let check = checkAST gtExpr (Array.toList fs.FConstantComps)

    match check with 
    | Properties p -> 
        let res = evaluate gtExpr fs 0
        match res with 
        | Bool b, Size s when b = false && s = 1 -> Ok("the AST was compiled and evaluated correctly")
        | Bool b, Size s -> Error($"returned the wrong result. {b} {s}")
        | t, Size s -> Error($"returned the wrong result type {t}")
    | ErrLst e -> Error($"the AST did not compile. There were the following errors: {e}")
