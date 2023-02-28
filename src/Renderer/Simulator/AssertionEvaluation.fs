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

//get FComponentId for component that is in the sheet where it's currently being simulated 
let getFComponentId label components = 
    let isRightComponent (comp: FastComponent) = 
        match comp.FLabel, comp.FType with 
        | labelComp, Viewer _ when labelComp = label-> Some(comp.fId)
        | _ -> None 
    let compId = 
        List.choose isRightComponent components 
        |> function 
            | a::[] -> a
            | _ -> failwithf "should not happen"
    compId

// assume that the AST is correct (as it will be checked upon creation of the component)
let rec evaluate (tree: ExprInfo) components (fs:FastSimulation) step: Value * Size =
    let resizeRes (size: Size) res = 
        match res, size with 
        | Int neg, Size s when neg < 0 -> Int (max neg (int (-(2. ** float (s- 1))))), size
        | Int pos, Size s -> Int (min pos (int (2. ** float (s- 1)) - 1)), size
        | Uint v, Size s -> Uint (min v (uint (2. ** float s) - 1u)), size  
        | _ -> res, size 

    // can definitely be improved and abstracted more (maybe put together unOp and binOp)
    let ExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fBool: Option<Functions>) ops=
        match ops with
        | BinOp(l, r) ->
            let leftRes, sizeL = evaluate l components fs step
            let rightRes, sizeR = evaluate r components fs step

            let value = 
                match leftRes, rightRes with
                | Int op1, Int op2 ->
                    match fInt with
                    | Some(ItB f) -> Bool(f op1 op2)
                    | Some(ItI f) -> Int(f op1 op2)
                    // here I can have both operands being int but maybe the function one that can only be applied to bools or stuff like this
                    | _ -> failwithf "should not happen"
                | Uint op1, Uint op2 ->
                    match fUint with
                    | Some(UtB f) -> Bool(f op1 op2)
                    | Some(UtU f) -> Uint(f op1 op2)
                    | _ -> failwithf "should not happen" 
                | Bool op1, Bool op2 ->
                    match fBool with
                    | Some(BtB f) -> Bool(f op1 op2)
                    | _ -> failwithf "should not happen" 
                | _ -> failwithf "should not happen" 
            resizeRes sizeL value 
        | UnOp op ->
            let opEvald, size = evaluate op components fs step

            match opEvald with
            | Int op ->
                match fInt with
                | Some(ItIUn f) -> Int(f op), size
                | _ -> failwithf "should not happen" 
            | Uint op ->
                match fUint with
                | Some(UtUUn f) -> Uint(f op), size
                | _ -> failwithf "should not happen" 
            | Bool op->
                match fBool with
                | Some(BtBUn f) -> Bool(f op), size
                | _ -> failwithf "should not happen" 
    
    match tree with
    | Lit lit, _ ->
        let value = 
            match lit with
            | Value (Int int)->  Int int
            | Value (Uint uint) -> Uint uint
            | Value (Bool bool) -> Bool bool
            | Id id -> 
                let fCompId = getFComponentId id components 
                let data = fs.getSimulationData step fCompId (OutputPortNumber 0)

                match data with 
                | Data{Dat = fb; Width = _} ->  
                    match fb with 
                    | Word w -> Uint w
                    | _ -> failwithf "not supported yet"
                | _ -> failwithf "should not happen"
        let _, size = getLitProperties components lit
        value, Size size

    | Cast c, _ ->
        match c with
        | ToSigned e -> cast e "int" components fs step// this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> cast e "uint" components fs step
        | ToBool e -> cast e "bool" components fs step

    | BusCast (destSize, e), _-> 
        let value, _ = evaluate e components fs step
        resizeRes (Size destSize) value

    | Add ops, _ -> ExprEval (Some(ItI (+))) (Some(UtU (+))) None ops  
    | Sub ops, _ -> ExprEval (Some(ItI (-))) (Some(UtU (-))) None ops  
    | Mul ops, _ -> ExprEval (Some(ItI (*))) (Some(UtU (*))) None ops  
    | Div ops, _ -> ExprEval (Some(ItI (/))) (Some(UtU (/))) None ops  
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
and cast expr castType components fs step =
    // cast per se can't fail, but the expression it's called on might, so we need to be able to propagate the error
    let castExprEvaluated, size= evaluate expr components fs step
    let value = 
        match castExprEvaluated, castType with
        | Int int, "uint" -> Uint(uint int)
        | Int int, "bool" -> Bool(intToBool int)
        | Bool bool, "uint" -> Uint(boolToUint bool)
        | Bool bool, "int" -> Int(boolToInt bool)
        | Uint uint, "int" -> Int(int uint)
        | Uint uint, "bool" -> Bool(uintToBool uint)
        | _, _ -> castExprEvaluated
    value, size
//maybe make r an option to reduce the code? if it's present do binary else unary


/// Represents a failed assertion
/// Cycle: int - represents an integer value that indicates the cycle number in which the assertion failed
/// FailureMessage: string - represents a string value that describes the reason for the assertion failure
/// Sheet: string - represents a string value that indicates the sheet on which the assertion failed
/// TODO:(djj120/DomJustice) Not final place or form Lu will probs have her own version of this struct somewhere
type FailedAssertion = {
    Cycle: int
    FailureMessage: string
    Sheet: string
}

//function created by Lu for now will have place holder of fake data
let evaluateAssertionsInWindow (startCycle : int) (endCycle : int) (fs: FastSimulation): FailedAssertion list =
    let evalTree step assertion = 
        let value, size = evaluate assertion.AST (Array.toList fs.FConstantComps) fs step
        match value with 
        | Bool true -> None 
        | Bool false -> Some {Cycle = step; FailureMessage = $"the assertion n {assertion.Id} was supposed to return true but it returned false"; Sheet = "i don't know yet"} 
        | _ -> failwithf "the top level expression should return a bool"
    let evalAllAssertions assertions n = 
        assertions
        |> List.choose (evalTree n)
    [startCycle..endCycle]
    |> List.collect (evalAllAssertions fs.Assertions)