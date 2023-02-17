module AssertionEvaluation
open AssertionTypes

type IntToBool = int -> int -> bool
type IntToInt = int -> int -> int
type UintToBool = uint -> uint -> bool
type UintToUint = uint -> uint -> uint
type BoolToBool = bool -> bool -> bool

type Functions =
    | ItB of IntToBool
    | ItI of IntToInt
    | UtB of UintToBool
    | UtU of UintToUint
    | BtB of BoolToBool

let boolToInt =
    function
    | true -> 1
    | false -> 0

let boolToUint =
    function
    | true -> uint 1
    | false -> uint 0

let intToBool n = if n = 0 then false else true
let uintToBool n = if n = uint 0 then false else true

type EvalRes = 
    | Err of Error 
    | Val of Value
let rec evaluate (tree: Expr) : EvalRes =
    match tree with
    | Lit (lit, pos) ->

        match lit with
        | Value value ->

            match value with
            | Int int -> Val (Int int)
            | Uint uint -> Val(Uint uint)
            | Bool bool -> Val(Bool bool)

        | Id id -> Val(Int 0) // not implemented yet

    | Cast (c, pos) ->

        match c with
        | ToSigned e -> Val(cast e "int") // this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> Val (cast e "uint")
        | ToBool e -> Val (cast e "bool")

    | Add (BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (+))) (Some(UtU (+))) None l r)

    | Sub(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (-))) (Some(UtU (-))) None l r)

    | Mul(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (*))) (Some(UtU (*))) None l r)

    | Div(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (/))) (Some(UtU (/))) None l r)

    | Rem(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (%))) (Some(UtU (%))) None l r)

    | BitAnd(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (&&&))) (Some(UtU (&&&))) None l r)

    | BitOr(BinOp(l, r), pos) -> Val(binExprEval (Some(ItI (|||))) (Some(UtU (|||))) None l r)

    | BitNot(UnOp(op), pos) -> unExprEval (~~~) (~~~) op

    | BoolExpr (boolExpr, pos)->

        match boolExpr with
        | Eq(BinOp(l, r)) -> Val(binExprEval (Some(ItB (=))) (Some(UtB (=))) (Some(BtB (=))) l r)
        | Neq(BinOp(l, r)) -> Val(binExprEval (Some(ItB (<>))) (Some(UtB (<>))) (Some(BtB (<>))) l r)
        | LogAnd(BinOp(l, r)) -> Val(binExprEval None None (Some(BtB (&&))) l r)
        | LogOr(BinOp(l, r)) -> Val(binExprEval None None (Some(BtB (||))) l r)
        | Lt(BinOp(l, r)) -> Val(binExprEval (Some(ItB (<))) (Some(UtB (<))) (Some(BtB (<))) l r)
        | Gt(BinOp(l, r)) -> Val(binExprEval (Some(ItB (>))) (Some(UtB (>))) (Some(BtB (>))) l r)
        | Gte(BinOp(l, r)) -> Val(binExprEval (Some(ItB (>=))) (Some(UtB (>=))) (Some(BtB (>=))) l r)
        | Lte(BinOp(l, r)) -> Val(binExprEval (Some(ItB (<=))) (Some(UtB (<=))) (Some(BtB (<=))) l r)
        | LogNot(UnOp op) ->
            let (Val evaluated) = evaluate op

            match evaluated with
            | Bool bool -> Val(Bool(not bool))
            | _ -> failwithf "wrong type, should be bool"
        | _ -> failwithf "not implemented yet"


    | _ -> Val(Int 0)

and cast (expr: Expr) castType : Value =
    let (Val evaldExpr) = evaluate expr

    match evaldExpr, castType with
    | Int int, "uint" -> Uint(uint int)
    | Int int, "bool" -> Bool(intToBool int)
    | Bool bool, "uint" -> Uint(boolToUint bool)
    | Bool bool, "int" -> Int(boolToInt bool)
    | Uint uint, "int" -> Int(int uint)
    | Uint uint, "bool" -> Bool(uintToBool uint)
    | _, _ -> evaldExpr

//maybe make r an option to reduce the code? if it's present do binary else unary
and binExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fBool: Option<Functions>) l r =
    let (Val left) = evaluate l
    let (Val right) = evaluate r

    match left, right with
    | Int op1, Int op2 ->
        match fInt with
        | Some(ItB f) -> Bool(f op1 op2)
        | Some(ItI f) -> Int(f op1 op2)
        | _ -> failwithf "wrong function type"
    | Uint op1, Uint op2 ->
        match fUint with
        | Some(UtB f) -> Bool(f op1 op2)
        | Some(UtU f) -> Uint(f op1 op2)
        | _ -> failwithf "wrong function type"
    | Bool op1, Bool op2 ->
        match fBool with
        | Some(BtB f) -> Bool(f op1 op2)
        | _ -> failwithf "wrong function type"
    | _ -> failwithf "invalid operation" // this will be paired with error messages

and unExprEval fInt fUint op =
    let (Val opEvald) = evaluate op

    match opEvald with
    | Int op -> Val(Int(fInt op))
    | Uint op -> Val(Uint(fUint op))
    | _ -> failwithf "invalid operation"
