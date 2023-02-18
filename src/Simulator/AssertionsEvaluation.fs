module AssertionEvaluation

open AssertionTypes

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

type EvalRes =
    | Err of Error list
    | Val of Value

let rec evaluate (tree: Expr) : EvalRes =
    let propagateError (leftRes: EvalRes) (rightRes: EvalRes) =
        let toErr =
            function
            | Val v -> []
            | Err e -> e

        toErr leftRes @ toErr rightRes

    let hetTypesErr () =
        "This function can't be applied on value of different types"

    let invTypesErr () = "Types not supported by this function"

    let makeTypeError errType leftType (rightType: string) pos =
        let msg = errType () + ". left expr is of type: " + leftType + if rightType.Length = 0 then "" else (". Right expr is of type: " + rightType)
        Err [ { Msg = msg; Pos = pos } ]

    let typeName =
        function
        | Int _ -> "int"
        | Uint _ -> "uint"
        | Bool _ -> "bool"

    // what is the difference between and and let inside the linked function
    // i think that it's better probably to do and (for efficiency reasons i wonder)
    let cast (expr: Expr) castType : EvalRes =
        // cast per se can't fail, but the expression it's called on might, so we need to be able to propagate the error
        match (evaluate expr) with
        | Val value ->
            match value, castType with
            | Int int, "uint" -> Val(Uint(uint int))
            | Int int, "bool" -> Val(Bool(intToBool int))
            | Bool bool, "uint" -> Val(Uint(boolToUint bool))
            | Bool bool, "int" -> Val(Int(boolToInt bool))
            | Uint uint, "int" -> Val(Int(int uint))
            | Uint uint, "bool" -> Val(Bool(uintToBool uint))
            | _, _ -> Val(value)
        | Err error -> Err error

    // can definitely be improved and abstracted more (maybe put together unOp and binOp)
    let ExprEval (fInt: Option<Functions>) (fUint: Option<Functions>) (fBool: Option<Functions>) ops pos =
        match ops with
        | BinOp(l, r) ->
            let leftRes = evaluate l
            let rightRes = evaluate r

            match leftRes, rightRes with
            | Val left, Val right ->
                match left, right with
                | Int op1, Int op2 ->
                    match fInt with
                    | Some(ItB f) -> Val(Bool(f op1 op2))
                    | Some(ItI f) -> Val(Int(f op1 op2))
                    // here I can have both operands being int but maybe the function one that can only be applied to bools or stuff like this
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "int" "int" pos
                | Uint op1, Uint op2 ->
                    match fUint with
                    | Some(UtB f) -> Val(Bool(f op1 op2))
                    | Some(UtU f) -> Val(Uint(f op1 op2))
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "int" "int" pos
                | Bool op1, Bool op2 ->
                    match fBool with
                    | Some(BtB f) -> Val(Bool(f op1 op2))
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "int" "int" pos
                | _ -> makeTypeError hetTypesErr (typeName left) (typeName right) pos // this will be paired with error messages
            | _ -> Err(propagateError leftRes rightRes)
        | UnOp op ->
            let opEvald = evaluate op

            match opEvald with
            | Val value ->
                match value with
                | Int op ->
                    match fInt with
                    | Some(ItIUn f) -> Val(Int(f op))
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "int" "" pos 
                | Uint op ->
                    match fUint with
                    | Some(UtUUn f) -> Val(Uint(f op))
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "uint" "" pos 
                | Bool op ->
                    match fBool with
                    | Some(BtBUn f) -> Val(Bool(f op))
                    | Some(_) -> failwithf "Dev error: the function was passed with the wrong wrapper. Debug."
                    | _ -> makeTypeError invTypesErr "bool" "" pos 
            | _ -> opEvald

    match tree with
    | Lit(lit, pos) ->
        match lit with
        | Value value ->
            match value with
            | Int int -> Val(Int int)
            | Uint uint -> Val(Uint uint)
            | Bool bool -> Val(Bool bool)
        | Id id -> Val(Int 0) // not implemented yet

    | Cast(c, pos) ->
        match c with
        | ToSigned e -> cast e "int" // this might require some sort of manipulation? or will it be done automatically
        | ToUnsigned e -> cast e "uint"
        | ToBool e -> cast e "bool"

    | Add(ops, pos) -> ExprEval (Some(ItI (+))) (Some(UtU (+))) None ops pos
    | Sub(ops, pos) -> ExprEval (Some(ItI (-))) (Some(UtU (-))) None ops pos
    | Mul(ops, pos) -> ExprEval (Some(ItI (*))) (Some(UtU (*))) None ops pos
    | Div(ops, pos) -> ExprEval (Some(ItI (/))) (Some(UtU (/))) None ops pos
    | Rem(ops, pos) -> ExprEval (Some(ItI (%))) (Some(UtU (%))) None ops pos
    | BitAnd(ops, pos) -> ExprEval (Some(ItI (&&&))) (Some(UtU (&&&))) None ops pos
    | BitOr(ops, pos) -> ExprEval (Some(ItI (|||))) (Some(UtU (|||))) None ops pos
    | BitNot(op, pos) -> ExprEval (Some(ItIUn(~~~))) (Some(UtUUn(~~~))) None op pos

    | BoolExpr(boolExpr, pos) ->
        match boolExpr with
        | Eq(ops) -> ExprEval (Some(ItB (=))) (Some(UtB (=))) (Some(BtB (=))) ops pos
        | Neq(ops) -> ExprEval (Some(ItB (<>))) (Some(UtB (<>))) (Some(BtB (<>))) ops pos
        | LogAnd(ops) -> ExprEval None None (Some(BtB (&&))) ops pos
        | LogOr(ops) -> ExprEval None None (Some(BtB (||))) ops pos
        | Lt(ops) -> ExprEval (Some(ItB (<))) (Some(UtB (<))) (Some(BtB (<))) ops pos
        | Gt(ops) -> ExprEval (Some(ItB (>))) (Some(UtB (>))) (Some(BtB (>))) ops pos
        | Gte(ops) -> ExprEval (Some(ItB (>=))) (Some(UtB (>=))) (Some(BtB (>=))) ops pos
        | Lte(ops) -> ExprEval (Some(ItB (<=))) (Some(UtB (<=))) (Some(BtB (<=))) ops pos
        | LogNot(op) -> ExprEval None None (Some(BtBUn (not) )) op pos

    | _ -> Val(Int 0)

//maybe make r an option to reduce the code? if it's present do binary else unary
