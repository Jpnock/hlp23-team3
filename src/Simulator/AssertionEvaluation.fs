module AssertionEvaluation
open AssertionTypes

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


// assume that the AST is correct (as it will be checked upon creation of the component)
let rec evaluate (tree: Expr) : Value= 

    // can definitely be improved and abstracted more (maybe put together unOp and binOp)
    let ExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fBool: Option<Functions>) ops=
        match ops with
        | BinOp(l, r) ->
            let leftRes = evaluate l
            let rightRes = evaluate r
            
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
        | UnOp op ->
            let opEvald = evaluate op

            match opEvald with
            | Int op ->
                match fInt with
                | Some(ItIUn f) -> Int(f op)
                | _ -> failwithf "should not happen" 
            | Uint op ->
                match fUint with
                | Some(UtUUn f) -> Uint(f op)
                | _ -> failwithf "should not happen" 
            | Bool op->
                match fBool with
                | Some(BtBUn f) -> Bool(f op)
                | _ -> failwithf "should not happen" 
    
    match tree with
    | Lit(lit, _) ->
        match lit with
        | Value (Int int)->  Int int
        | Value (Uint uint) -> Uint uint
        | Value (Bool bool) -> Bool bool
        | Id id -> Int 0 // not implemented yet

    | Cast(c, _) ->
        match c with
        | ToSigned e -> cast e "int" // this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> cast e "uint"
        | ToBool e -> cast e "bool"

    | Add(ops, _) -> ExprEval (Some(ItI (+))) (Some(UtU (+))) None ops 
    | Sub(ops, _) -> ExprEval (Some(ItI (-))) (Some(UtU (-))) None ops 
    | Mul(ops, _) -> ExprEval (Some(ItI (*))) (Some(UtU (*))) None ops 
    | Div(ops, _) -> ExprEval (Some(ItI (/))) (Some(UtU (/))) None ops 
    | Rem(ops, _) -> ExprEval (Some(ItI (%))) (Some(UtU (%))) None ops 
    | BitAnd(ops, _) -> ExprEval (Some(ItI (&&&))) (Some(UtU (&&&))) None ops 
    | BitOr(ops, _) -> ExprEval (Some(ItI (|||))) (Some(UtU (|||))) None ops 
    | BitNot(op, _) -> ExprEval (Some(ItIUn(~~~))) (Some(UtUUn(~~~))) None op 

    | BoolExpr(boolExpr, _) ->
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

    | _ -> Int 0

    // what is the difference between and and let inside the linked function
    // i think that it's better probably to do and (for efficiency reasons i wonder)
and cast expr castType=
    // cast per se can't fail, but the expression it's called on might, so we need to be able to propagate the error
    let castExprEvaluated = evaluate expr
    match castExprEvaluated, castType with
    | Int int, "uint" -> Uint(uint int)
    | Int int, "bool" -> Bool(intToBool int)
    | Bool bool, "uint" -> Uint(boolToUint bool)
    | Bool bool, "int" -> Int(boolToInt bool)
    | Uint uint, "int" -> Int(int uint)
    | Uint uint, "bool" -> Bool(uintToBool uint)
    | _, _ -> castExprEvaluated
//maybe make r an option to reduce the code? if it's present do binary else unary
