//authored by ln220
module AssertionEvaluation

open AssertionTypes
open AssertionCheck
open CommonTypes 
open SimulatorTypes

// these types are here because they concern evaluation, not assertions in general
type IntToBool = int -> int -> bool
type IntToInt = int -> int -> int
type UintToBool = uint -> uint -> bool
type UintToUint = uint -> uint -> uint
type BoolToBool = bool -> bool -> bool
type IntToIntUn = int -> int
type UintToUintUn = uint -> uint
type BoolToBoolUn = bool -> bool

type Functions =
    | ItB of IntToBool
    | ItI of IntToInt
    | UtB of UintToBool
    | UtU of UintToUint
    | BtB of BoolToBool
    | ItIUn of IntToIntUn
    | UtUUn of UintToUintUn
    | BtBUn of BoolToBoolUn

let boolToInt =
    function
    | true -> 1
    | false -> 0

let boolToUint =
    function
    | true -> 1u
    | false -> 0u

let intToBool n = if n = 0 then false else true
let uintToBool n = if n = uint 0 then false else true

/// get FComponentId for component that is in the sheet where it's currently being simulated 
let getFComponentId label components = 
    let isRightComponent (comp: FastComponent) = 
        match comp.FLabel with 
        | labelComp when labelComp = label -> Some(comp.fId)
        | _ -> None
    let compId = 
        List.choose isRightComponent components 
        |> function 
            | [a] -> a
            | _ -> failwithf "should not happen"
    compId

let getLitType (components: FastComponent List) lit = 
    match lit with
    | Value value -> getType value
    | Id _ -> UintType // the existance of the id has been checked in the compilation

// assume that the AST is correct (as it will be checked upon creation of the component)
let rec evaluate (tree: ExprInfo) (fs:FastSimulation) step: Result<Value, string> =
    let resizeRes (size: Size) res = 
        match res, size with 
        | Int neg, Size s when neg < 0 -> Int (max neg (int (-(2. ** float (s- 1)))))
        | Int pos, Size s -> Int (min pos (int (2. ** float (s- 1)) - 1))
        | Uint v, Size s -> Uint (min v (uint (2. ** float s) - 1u))
        | _ -> res

    // can definitely be improved and abstracted more (maybe put together unOp and binOp)
    let ExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fBool: Option<Functions>) ops=
        match ops with
        | BinOp(l, r) ->
            let leftRes = evaluate l fs step
            let rightRes= evaluate r fs step
            printf "evaluated %A %A" leftRes rightRes
            printf "from %A %A" l r

            let value = 
                match leftRes, rightRes with
                // all this repeated code is required as type inference does not allow to put the different conditions as the same one 
                | Ok(Int op1), Ok(Int op2) -> 
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f  op1 op2))
                    | Some(ItI f) -> Ok(Int(f op1 op2))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Int op1), Ok(Uint op2) -> 
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f op1 (int op2)))
                    | Some(ItI f) -> Ok(Int(f op1 (int op2)))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Uint op1), Ok(Int op2) ->
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f (int op1) op2))
                    | Some(ItI f) -> Ok(Int(f (int op1) op2))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Uint op1), Ok(Uint op2) ->
                    match fUint with
                    | Some(UtB f) -> Ok(Bool(f op1 op2))
                    | Some(UtU f) -> Ok(Uint(f op1 op2))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Bool op1), Ok(Bool op2) ->
                    match fBool with
                    | Some(BtB f) -> Ok(Bool(f op1 op2))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Error(e1), Error(e2) -> Error(e1+e2)
                | Error(e1), _ -> Error(e1)
                | _, Error(e2)  -> Error(e2)
            value 
        | UnOp op ->
            let opEvald = evaluate op fs step

            match opEvald with
            | Ok(Int op) ->
                match fInt with
                | Some(ItIUn f) -> Ok(Int(f op))
                | _ -> Error("should not happen")
            | Ok(Uint op) ->
                match fUint with
                | Some(UtUUn f) -> Ok(Uint(f op))
                | _ ->Error("should not happen" )
            | Ok(Bool op)->
                match fBool with
                | Some(BtBUn f) -> Ok(Bool(f op))
                | _ -> Error("should not happen")
    
    match tree with
    | Lit lit, _ ->
        let value = 
            match lit with
            | Value (Int int)->  Int int
            | Value (Uint uint) -> Uint uint
            | Value (Bool bool) -> Bool bool
            | Id (id, portNumber) -> 
                let fCompId = getFComponentId id (List.ofSeq fs.FComps.Values)
                let data = fs.getSimulationData step fCompId (OutputPortNumber portNumber)
                match data with 
                | Data{Dat = fb; Width = _} ->  
                    match fb with 
                    | Word w -> Uint w
                    | _ -> failwithf "not supported yet"
                | _ -> failwithf "should not happen"
        Ok(value)

    | Cast c, _ ->
        match c with
        | ToSigned e -> cast e "int" fs step// this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> cast e "uint" fs step
        | ToBool e -> cast e "bool" fs step

    | BusCast (destSize, e), _-> 
        let value= evaluate e fs step
        value

    | Add ops, _ -> ExprEval (Some(ItI (+))) (Some(UtU (+))) None ops  
    | Sub ops, _ -> ExprEval (Some(ItI (-))) (Some(UtU (-))) None ops  
    | Mul ops, _ -> ExprEval (Some(ItI (*))) (Some(UtU (*))) None ops  
    | Div ops, _ -> 
        match ops with 
        | BinOp (l, r) -> 
            let left = evaluate l fs step 
            let right = evaluate r fs step 
            match left, right with 
            | _, Ok(Int 0) -> Error("division by 0 attempted")
            | _, Ok(Uint 0u) -> Error("division by 0 attempted")
            | _, _ -> ExprEval (Some(ItI (/))) (Some(UtU (/))) None ops  
    | Rem ops, _ -> ExprEval (Some(ItI (%))) (Some(UtU (%))) None ops  
    | BitAnd ops, _ -> ExprEval (Some(ItI (&&&))) (Some(UtU (&&&))) None ops 
    | BitOr ops, _ -> ExprEval (Some(ItI (|||))) (Some(UtU (|||))) None ops 
    | BitNot op, _ -> ExprEval (Some(ItIUn(~~~))) (Some(UtUUn(~~~))) None op 

    | BoolExpr boolExpr, _ ->
        match boolExpr with
        | Eq(ops) -> ExprEval (Some(ItB (=))) (Some(UtB (=))) (Some(BtB (=))) ops 
        | Neq(ops) -> ExprEval (Some(ItB (<>))) (Some(UtB (<>))) (Some(BtB (<>))) ops 
        | LogAnd(ops) -> ExprEval None None (Some(BtB (&&))) ops 
        | LogOr(ops) -> ExprEval None None (Some(BtB (||))) ops 
        | Lt(ops) -> ExprEval (Some(ItB (<))) (Some(UtB (<))) (Some(BtB (<))) ops 
        | Gt(ops) -> ExprEval (Some(ItB (>))) (Some(UtB (>))) (Some(BtB (>))) ops 
        | Gte(ops) -> ExprEval (Some(ItB (>=))) (Some(UtB (>=))) (Some(BtB (>=))) ops 
        | Lte(ops) -> ExprEval (Some(ItB (<=))) (Some(UtB (<=))) (Some(BtB (<=))) ops 
        | LogNot(op) -> ExprEval None None (Some(BtBUn (not) )) op 

    // what is the difference between and and let inside the linked function
    // i think that it's better probably to do and (for efficiency reasons i wonder)
and cast expr castType fs step=
    // cast per se can't fail, but the expression it's called on might, so we need to be able to propagate the error
    let castExprEvaluated= evaluate expr fs step
    let value = 
        match castExprEvaluated, castType with
        | Ok(Int int), "uint" -> Ok(Uint(uint int))
        | Ok(Int int), "bool" -> Ok(Bool(intToBool int))
        | Ok(Bool bool), "uint" -> Ok(Uint(boolToUint bool))
        | Ok(Bool bool), "int" -> Ok(Int(boolToInt bool))
        | Ok(Uint uint), "int" -> Ok(Int(int uint))
        | Ok(Uint uint), "bool" -> Ok(Bool(uintToBool uint))
        | _, _ -> castExprEvaluated
    value
//maybe make r an option to reduce the code? if it's present do binary else unary


/// Represents a failed assertion
/// Cycle: int - represents an integer value that indicates the cycle number in which the assertion failed
/// FailureMessage: string - represents a string value that describes the reason for the assertion failure
/// Sheet: string - represents a string value that indicates the sheet on which the assertion failed
/// Authored by djj120
type FailedAssertion = {
    Cycle: int
    FailureMessage: string
    Sheet: string
}

//function created by Lu for now will have place holder of fake data
let evaluateAssertionsInWindow (startCycle : int) (endCycle : int) (fs: FastSimulation): FailedAssertion list =
    let evalTree step (assertion:Assertion) = 
        let value= evaluate assertion.AssertExpr fs step
        match value with 
        | Ok(Bool true) -> None 
        | Ok(Bool false) ->
            let prettyAST = AssertionParser.prettyPrintAST (fst assertion.AssertExpr) "" false
            Some {Cycle = step; FailureMessage = $"The assertion \n{prettyAST}\nwas supposed to return true but it returned false\n"; Sheet = "Not implemented"} 
        | Ok(e) -> 
            let prettyAST = AssertionParser.prettyPrintAST (fst assertion.AssertExpr) "" false
            Some {Cycle = step; FailureMessage = $"The assertion \n{prettyAST}\nwas supposed to return a bool but it returned: {e}s\n"; Sheet = "Not implemented"} 
        | Error(e) -> 
            let prettyAST = AssertionParser.prettyPrintAST (fst assertion.AssertExpr) "" false
            Some {Cycle = step; FailureMessage = $"There was a problem with the evaluation of the assertion \n{prettyAST}\n {e}\n"; Sheet = "Not implemented"} 
    let evalAllAssertions assertions n = 
        assertions
        |> List.choose (evalTree n)
    [startCycle..endCycle]
    |> List.collect (evalAllAssertions fs.Assertions)

/// return the integers of the cycles with failed assertions given a list of failed assertions
/// Authored by djj120
let getFailedAssertionCycles (failedAssertions: FailedAssertion list)= 
    failedAssertions
    |> List.map (fun assertion -> assertion.Cycle)
    |> List.distinct

/// returns a list oc the failed assertions occurring at the current clk cycle specified in a 
/// SimulationData's FastSim
/// Authored by djj120
let getCurrAssertionFailuresStepSim (simData : SimulationData) =
    let failedAssertions = evaluateAssertionsInWindow simData.ClockTickNumber simData.ClockTickNumber simData.FastSim
    List.filter (fun assertion -> assertion.Cycle = simData.ClockTickNumber) failedAssertions
