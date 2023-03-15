//authored by ln220
module AssertionCheck

open AssertionTypes
open CommonTypes
open SimulatorTypes
open System

let (|RequiresBool|_|) (expr: ExprInfo) = 
    match expr with 
    | (BoolExpr (LogAnd(BinOp(l, r)))), pos| BoolExpr(LogOr (BinOp(l, r))), pos -> Some(l, r, pos)
    | _ -> None 

// no checks needed, return the type of the expression
let (|IsUnary|_|) (expr: ExprInfo) =
    match expr with 
    | BitNot(UnOp(op)), _ | BoolExpr(LogNot(UnOp(op))), _ -> Some(op)
    | _ -> None

// this is useful as it allows to neatly extract the expressions from the wrappers and check those directly 
// without having to match and repeat code in the main function 
let (|IsBoolExpr|_|) (expr: ExprInfo) = 
    match expr with 
    | BoolExpr(boolExpr), pos ->
        match boolExpr with 
        | Eq(BinOp(l, r)) | Neq(BinOp(l, r)) | Lt(BinOp(l, r)) | Gt(BinOp(l, r)) | Lte(BinOp(l,r)) | Gte(BinOp(l,r)) -> Some(l, r, pos)
        | _ -> None 
    | _ -> None

let (|IsBinExpr|_|) (expr: ExprInfo) = 
    match expr with 
    | Add(BinOp(l, r)), pos | Sub(BinOp(l, r)), pos | Mul(BinOp(l, r)), pos | Div(BinOp(l, r)), pos| Rem(BinOp(l, r)), pos -> Some(l, r, pos)
    | _ -> None

let getType value = 
    match value with 
    | Int _ -> IntType
    | Uint _ -> UintType 
    | Bool _ -> BoolType 

// TODO ln220 move the check for the existance of the id somewhere else 
/// get size of lit, if the id is not found in the schematics returns an error
let checkLitExistance (components: Component List) (lit: Lit) : Result<AssertionTypes.Type, string> = 
    match lit with
    | Value value -> Ok(getType value)
    | Id (id, _, _) -> 
        let isRightComponent (comp: Component) = 
            match comp.Label with 
            | idComp when idComp = id -> true
            | _ -> false 

        List.filter isRightComponent components 
        |> function 
            | [c] -> Ok(UintType) 
            | [] -> Error($"the ID {id} does not exist") 
            | _ -> failwithf "there are one or more components that match this description (should not happen, dev error not user error)"


/// check that the verification AST obeys the type system 
let rec checkAST (tree: ExprInfo) (components: Component List): CheckRes = 
    let propagateError (leftRes: CheckRes) (rightRes: CheckRes) =
        let toErr =
            function
            | TypeInfo _ -> []
            | ErrLst e -> e

        toErr leftRes @ toErr rightRes
    
    let hetTypesErr () =
        "This function can't be applied on a bool variable and a non-bool variable"

    let invTypesErr () = "Types not supported by this function"
    
    let makeTypeError errType leftType (rightType: Option<AssertionTypes.Type>) pos =
        let msg = errType () + ". left expr is of type: " + string leftType + if rightType.IsNone  then "" else (". Right expr is of type: " + string (Option.defaultValue UintType rightType))
        ErrLst [ { Msg = msg; Pos = pos } ]

    //let checkCast (castExpr: ExprInfo) castType (castSize: Option<int>) pos = 
    let checkCast (castExpr: ExprInfo) castType (castSize: Option<int>) pos = 
        let exprRes = checkAST castExpr components
        match exprRes with 
        | TypeInfo BoolType when castSize.IsSome -> 
            makeTypeError invTypesErr BoolType None pos
        | TypeInfo t -> 
            TypeInfo (Option.defaultValue t castType)
        | _ -> exprRes //propagate error


    let checkBin l r pos supportsBool makesBool =
        let leftChecked = checkAST l components
        let rightChecked= checkAST r components  
        match leftChecked, rightChecked with
        | TypeInfo typeL, TypeInfo typeR -> 
            if typeL = typeR && typeL = BoolType
            then
                if supportsBool 
                then leftChecked
                else makeTypeError invTypesErr typeL (Some typeR) pos
            elif typeL <> BoolType && typeR <> BoolType 
            //then checkSize l r leftChecked rightChecked pos makesBool
            then leftChecked 
            else makeTypeError hetTypesErr typeL (Some typeR) pos //not same type error
        | _ ->  ErrLst (propagateError leftChecked rightChecked)

    match tree with
    | IsUnary op -> 
        let opRes = checkAST op components
        match opRes with 
        | TypeInfo _ -> opRes 
        | _ -> failwithf "should not happen" 
    | RequiresBool (l, r, pos) -> // check that operand(s) are bool 
        let leftRes = checkAST l components
        let rightRes = checkAST r components 
        match leftRes, rightRes with 
        | TypeInfo typeL, TypeInfo typeR -> 
            if typeL = BoolType && typeL = typeR 
            then leftRes // it's not important what is passed, it's enough to pass type and size information 
            else makeTypeError invTypesErr typeL (Some typeR) pos
        | _ -> ErrLst (propagateError leftRes rightRes)
    | IsBoolExpr (l, r, pos) -> checkBin l r pos true true
    | IsBinExpr (l, r, pos) -> checkBin l r pos false false
    | Lit lit, pos -> 
        match checkLitExistance components lit with 
            | Ok(litType) -> TypeInfo litType
            | Error(msg) -> ErrLst [ { Msg = msg; Pos = pos } ]
    | Cast cast, pos -> 
        match cast with
        | ToSigned expr -> checkCast expr (Some IntType) None pos
        | ToUnsigned expr -> checkCast expr (Some UintType) None pos
        | ToBool expr-> checkCast expr (Some BoolType) None pos
    | BusCast(newSize, expr), pos -> checkCast expr None (Some newSize) pos
    | _ -> failwithf "should not happen" // check that operands (do nothing for unary operators) are the same and that their size is the same
