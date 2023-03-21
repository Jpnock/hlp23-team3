//authored by ln220
module AssertionEvaluation

open AssertionTypes
open AssertionCheck
open CommonTypes 
open SimulatorTypes
open BusWidthInferer
open System

type IntToBool = int64 -> int64 -> bool
type IntToInt = int64 -> int64 -> int64
type UintToBool = uint64 -> uint64 -> bool
type UintToUint = uint64 -> uint64 -> uint64
type BoolToBool = bool -> bool -> bool
type IntToIntUn = int64 -> int64
type UintToUintUn = uint64 -> uint64
type BoolToBoolUn = bool -> bool
type FloatToFloat = float32->float32->float32
type FloatToBool = float32->float32->bool

type Functions =
    | ItB of IntToBool
    | ItI of IntToInt
    | UtB of UintToBool
    | UtU of UintToUint
    | BtB of BoolToBool
    | ItIUn of IntToIntUn
    | UtUUn of UintToUintUn
    | BtBUn of BoolToBoolUn
    | FtF of FloatToFloat 
    | FtB of FloatToBool

let boolToInt =
    function
    | true -> 1
    | false -> 0

let boolToUint =
    function
    | true -> 1UL
    | false -> 0UL

let intToBool n = not (n = 0L)
let uintToBool n = not (n = 0UL)

// Converts bits to a float32
let nToFloat32 (n: uint64) : float32 =
    let exponent = (n >>> 23) &&& uint64 0xFF |> int
    match exponent with
    | 0 ->
        // De-normals are zero (DAZ) for this implementation
        0.0f
    | _ ->
        let mantissa = n &&& uint64 0x7FFFFF |> int
        let sign = n >>> 31 |> int
        
        let trueSign = if sign = 0 then 1.0f else -1.0f
        let trueExp = float32(exponent - 127)
        let trueMantissa = (1.0f + float32(mantissa) / float32(0x800000))
        trueSign * (2.0f ** trueExp) * trueMantissa

/// get the smallest possible number of bits that a lit would need, 
/// this provides the width for literals that are not buses
let getLitMinSize lit = 
    match lit with
    | Int intN -> 
        let n = float intN
        (ceil >> int) (Math.Log (n, 2.) + 0.1) + 1
    | Uint uint -> 
        let n = float uint 
        (ceil >> int) (Math.Log (n, 2.) + 0.1)
    | Bool bool -> 1
    | Float f -> 32


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

/// extract type information from Lit 
let getLitType (components: FastComponent List) lit = 
    match lit with
    | Value value -> getType value
    | Id _ -> UintType // the existance of the id has been checked in the compilation

///create bitwise mask of size n
let mask n : uint64 = 
    uint64 ((Math.Pow (2.0, float n)) - 1.0)

/// given a width, sign extend the number so that it can be held in an int64 while maintaining the correct value.
/// This function is invoked before returning a result to ensure operands have a value coherent with their width and 
/// signedness before being used in the evaluation of the driven block
let resizeSigned (n: int64) width =
    let isNegative = ((n >>> (width-1)) &&& 0b1) = 1
    let signExtended = if isNegative then (-1L <<< width) ||| n else n
    signExtended

let handleFP op1 op2 operand =
    let reverseIf cond li =
        if cond then Array.rev li else li    

    let fRes: float32 = operand op1 op2
    
    let fBytes =
        System.BitConverter.GetBytes(fRes)
        |> reverseIf (not BitConverter.IsLittleEndian)
    
    let castAndShift value shiftAmount =
       (uint64 value) <<< shiftAmount
       
    [0..8..24]
    |> List.mapi (fun i -> castAndShift fBytes[i])
    |> List.reduce (|||)
    |> uint64

 // make it convert it back to uint64
// assume that the AST is correct (as it will be checked upon creation of the component)
/// evaluate an assertion in a given cycle. Uses simulation data and up to date width inference information
let rec evaluate (tree: ExprInfo) (fs:FastSimulation) step (connectionsWidth: ConnectionsWidth): Result<Value * Size, string> =
   
    /// evaluate a binary or unary expression
    let ExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fFloat: Option<Functions>) (fBool: Option<Functions>) ops=
        match ops with
        | BinOp(l, r) ->
            let leftRes= evaluate l fs step connectionsWidth 
            let rightRes= evaluate r fs step connectionsWidth
            printf "evaluated %A %A" leftRes rightRes
            printf "from %A %A" l r
            
            let value = 
                match leftRes, rightRes with
                // all this repeated code is required as type inference does not allow to put the different conditions as the same one 
                | Ok(Int op1, Size sizeL), Ok(Int op2, Size sizeR) -> 
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f  op1 op2), Size 1)
                    | Some(ItI f) -> Ok(Int(resizeSigned (f op1 op2) (max sizeL sizeR)), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Int op1, Size sizeL), Ok(Uint op2, Size sizeR) -> 
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f op1 (int op2)), Size 1)
                    | Some(ItI f) -> Ok(Int( resizeSigned (f op1 (int op2)) (max sizeL sizeR)), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Uint op1, Size sizeL), Ok(Int op2, Size sizeR) ->
                    match fInt with
                    | Some(ItB f) -> Ok(Bool(f (int op1) op2), Size(1))
                    | Some(ItI f) -> Ok(Int( resizeSigned (f (int op1) op2) (max sizeL sizeR)), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Uint op1, Size sizeL), Ok(Uint op2, Size sizeR) ->
                    match fUint with
                    | Some(UtB f) -> Ok(Bool(f op1 op2), Size(1))
                    | Some(UtU f) -> Ok(Uint(f op1 op2 &&& (mask (max sizeL sizeR))), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Bool op1, Size sizeL), Ok(Bool op2, Size sizeR) ->
                    match fBool with
                    | Some(BtB f) -> Ok(Bool(f op1 op2), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok (Float op1, Size sizeL), Ok (Float op2, Size sizeR) ->
                    match fFloat with 
                    | Some(FtB f) -> Ok(Bool( f (nToFloat32 op1) (nToFloat32 op2)), Size(max sizeL sizeR))
                    | Some(FtF f) -> Ok(Uint(handleFP (nToFloat32 op1) (nToFloat32 op2) f), Size(max sizeL sizeR))
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Float opF, sizeF), Ok(Uint opU, _) -> 
                    match fFloat with 
                    | Some (FtB f) -> Ok(Bool(f (nToFloat32 opF) (float32 opU)), sizeF)
                    | Some (FtF f) -> Ok(Uint(handleFP (nToFloat32 opF) (float32 opU) f), sizeF)
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Ok(Uint opU, sizeU), Ok (Float opF, sizeF) -> 
                    match fFloat with 
                    | Some(FtB f) -> Ok(Bool(f (float32 opU) (nToFloat32 opF)), sizeF)
                    | Some(FtF f) -> Ok(Uint (handleFP (float32 opU) (nToFloat32 opF) f), sizeF)
                    | _ -> Error("Dev error: no function provided for the needed type")
                | Error(e1), Error(e2) -> Error(e1+e2)
                | Error(e1), _ -> Error(e1)
                | _, Error(e2)  -> Error(e2)
                | _ -> Error("Dev error")
            value 
        | UnOp op ->
            let opEvald = evaluate op fs step connectionsWidth

            match opEvald with
            | Ok(Int op, size) ->
                match fInt with
                | Some(ItIUn f) -> Ok(Int(f op), size)
                | _ -> Error("should not happen")
            | Ok(Uint op, size) ->
                match fUint with
                | Some(UtUUn f) -> Ok(Uint(f op), size)
                | _ ->Error("should not happen" )
            | Ok(Bool op, size)->
                match fBool with
                | Some(BtBUn f) -> Ok(Bool(f op), Size 1)
                | _ -> Error("should not happen")
            | _ -> Error("should not happen")
    
    match tree with
    | Lit lit, _ ->
        let value = 
            match lit with
            | Value (Int int)->  Ok(Int int, Size (getLitMinSize (Int int)))
            | Value (Uint uint) -> Ok(Uint uint, Size (getLitMinSize (Uint uint)))
            | Value (Bool bool) -> Ok(Bool bool, Size 1)
            | Value (Float uint) -> Ok(Float uint, Size (getLitMinSize (Uint uint)))
            | Id (id, portNumber, connId) -> 
                let fCompId = getFComponentId id (List.ofSeq fs.FComps.Values)
                let data = fs.getSimulationData step fCompId (OutputPortNumber portNumber)
                let size = connectionsWidth[ConnectionId connId]
                match data, size with 
                | Data{Dat = fb; Width = _}, Some(width) ->  
                    match fb with 
                    | Word w -> Ok(Uint (uint64 w), Size width)
                    | _ -> Error("failed to retrieve width or value of literal")
                | _ -> Error("dev error")
        value

    | Cast c, _ ->
        match c with
        | ToSigned e -> cast e "int" fs step connectionsWidth// this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> cast e "uint" fs step connectionsWidth
        | ToBool e -> cast e "bool" fs step connectionsWidth
        | ToFloat e -> cast e "float32" fs step connectionsWidth

    | BusCast (destSize, e), _-> 
        let value= evaluate e fs step connectionsWidth
        value

    | Add ops, _ -> ExprEval (Some(ItI (+))) (Some(UtU (+))) (Some(FtF (+))) None ops  
    | Sub ops, _ -> ExprEval (Some(ItI (-))) (Some(UtU (-))) (Some(FtF (-))) None ops  
    | Mul ops, _ -> ExprEval (Some(ItI (*))) (Some(UtU (*))) (Some(FtF (*))) None ops  
    | Div ops, _ -> 
        match ops with 
        | BinOp (l, r) -> 
            let left = evaluate l fs step connectionsWidth
            let right = evaluate r fs step connectionsWidth 
            match left, right with 
            | _, Ok(Int 0L, _) -> Error("division by 0 attempted")
            | _, Ok(Uint 0UL, _) -> Error("division by 0 attempted")
            | _, _ -> ExprEval (Some(ItI (/))) (Some(UtU (/))) (Some(FtF(/))) None ops 
        | UnOp _ -> Error("should not be a unary operator") 
    | Rem ops, _ -> ExprEval (Some(ItI (%))) (Some(UtU (%)))(Some(FtF(%))) None ops  
    | BitAnd ops, _ -> ExprEval (Some(ItI (&&&))) (Some(UtU (&&&))) None None ops 
    | BitOr ops, _ -> ExprEval (Some(ItI (|||))) (Some(UtU (|||))) None None ops 
    | BitNot op, _ -> ExprEval (Some(ItIUn(~~~))) (Some(UtUUn(~~~))) None None op 

    | BoolExpr boolExpr, _ ->
        match boolExpr with
        | Eq(ops) -> ExprEval (Some(ItB (=))) (Some(UtB (=))) (Some(FtB (=))) (Some(BtB (=))) ops 
        | Neq(ops) -> ExprEval (Some(ItB (<>))) (Some(UtB (<>))) (Some(FtB (<>)))(Some(BtB (<>))) ops 
        | LogAnd(ops) -> ExprEval None None None (Some(BtB (&&))) ops 
        | LogOr(ops) -> ExprEval None None None (Some(BtB (||))) ops 
        | Lt(ops) -> ExprEval (Some(ItB (<))) (Some(UtB (<))) (Some(FtB (<))) (Some(BtB (<))) ops 
        | Gt(ops) -> ExprEval (Some(ItB (>))) (Some(UtB (>))) (Some(FtB(>))) (Some(BtB (>))) ops 
        | Gte(ops) -> ExprEval (Some(ItB (>=))) (Some(UtB (>=))) (Some(FtB(>=))) (Some(BtB (>=))) ops 
        | Lte(ops) -> ExprEval (Some(ItB (<=))) (Some(UtB (<=))) (Some(FtB(>=))) (Some(BtB (<=))) ops 
        | LogNot(op) -> ExprEval None None None (Some(BtBUn (not) )) op 

    // what is the difference between and and let inside the linked function
    // i think that it's better probably to do and (for efficiency reasons i wonder)
/// evaluates results of type casting functions 
and cast expr castType fs step connectionsWidth=
    // cast per se can't fail, but the expression it's called on might, so we need to be able to propagate the error
    let castExprEvaluated= evaluate expr fs step connectionsWidth
    let value = 
        match castExprEvaluated, castType with
        | Ok(Uint uint, Size size), "float32" -> (Float(uint), Size 32) |> Ok
        | Ok(Int int, Size size), "float32" -> (Float(uint64 int), Size 32) |> Ok
        | Ok(Int int, size ), "uint" -> Ok(Uint(uint64 (int &&& -1L)), size)
        | Ok(Int int, size), "bool" -> Ok(Bool(intToBool int), size)
        | Ok(Bool bool, size), "uint" -> Ok(Uint(boolToUint bool), size)
        | Ok(Bool bool, Size size), "int" -> Ok(Int(resizeSigned (boolToInt bool) size), Size size)
        | Ok(Uint uint, Size size), "int" -> Ok(Int(resizeSigned (int uint) size), Size size)
        | Ok(Uint uint, size), "bool" -> Ok(Bool(uintToBool uint), size)
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
/// evaluates all assertions in a certain clock cycles window
let evaluateAssertionsInWindow (startCycle : int) (endCycle : int) (fs: FastSimulation): FailedAssertion list =
    let connectionsWidth: ConnectionsWidth= 
        fs.SimulatedCanvasState
        |> List.map (fun lC -> inferConnectionsWidth lC.CanvasState)
        |> List.collect (function 
            | Ok cw -> [cw] 
            | Error _ -> []) 
        |> List.collect (fun map -> Map.toList map)
        |> Map.ofList
        // need to collect the maps in one map

    let evalTree step assertion = 
        let value = evaluate assertion.AssertExpr fs step connectionsWidth 
        match value with 
        | Ok(Bool true, _) -> None 
        | Ok(Bool false, _) ->
            let prettyAST = AssertionParser.prettyPrintAST (fst assertion.AssertExpr) "" false
            Some {Cycle = step; FailureMessage = $"The assertion \n{prettyAST}\nwas supposed to return true but it returned false\n"; Sheet = "Not implemented"} 
        | Ok(e, _) -> 
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
