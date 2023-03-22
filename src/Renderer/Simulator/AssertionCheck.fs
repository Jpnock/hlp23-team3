//authored by ln220
module AssertionCheck

open AssertionTypes
open CommonTypes
open SimulatorTypes
open System

/// Facilitates accessing expressions with similar characteristics under a type-system 
/// point of view to facilitate compiler checks
let (|RequiresBool|_|) (expr: ExprInfo) = 
    match expr with 
    | (BoolExpr (LogAnd(BinOp(l, r)))), pos| BoolExpr(LogOr (BinOp(l, r))), pos -> Some(l, r, pos)
    | _ -> None 

// no checks needed, return the type of the expression
/// Allows to match on all unary expressions
let (|IsUnary|_|) (expr: ExprInfo) =
    match expr with 
    | BitNot(UnOp(op)), _ | BoolExpr(LogNot(UnOp(op))), _ -> Some(op)
    | _ -> None

// this is useful as it allows to neatly extract the expressions from the wrappers and check those directly 
// without having to match and repeat code in the main function 
/// Allows to match on all expressions that return a bool
let (|IsBoolExpr|_|) (expr: ExprInfo) = 
    match expr with 
    | BoolExpr(boolExpr), pos ->
        match boolExpr with 
        | Eq(BinOp(l, r))
        | Neq(BinOp(l, r))
        | Lt(BinOp(l, r))
        | Gt(BinOp(l, r))
        | Lte(BinOp(l,r))
        | Gte(BinOp(l,r)) -> Some(l, r, pos)
        | _ -> None
    | _ -> None

/// Allows to match on all binary expression 
let (|IsBinExpr|_|) (expr: ExprInfo) = 
    match expr with 
    | Add(BinOp(l, r)), pos
    | Sub(BinOp(l, r)), pos
    | Mul(BinOp(l, r)), pos
    | Div(BinOp(l, r)), pos
    | Rem(BinOp(l, r)), pos
    | BitAnd(BinOp(l,r)), pos
    | BitOr(BinOp(l,r)), pos -> Some(l, r, pos)
    | _ -> None

let getType value = 
    match value with 
    | Int _ -> IntType
    | Uint _ -> UintType 
    | Bool _ -> BoolType 
    | Float _ -> FloatType

/// Check if the lit name that is being used refers to an existing component.
/// Mainly relevant for text based assertions
let checkLitExistance (sheetComps: Map<string, Component list>) (lit: Lit) : Result<AssertionTypes.Type, string> = 
    match lit with
    | Value value -> Ok(getType value)
    | Id (label, sheet, _, _) -> Ok(UintType)
        (*
        let isRightComponent (comp: Component) = 
            match comp.Label with 
            | idComp when idComp = label -> true
            | _ -> false
        sheetComps
        |> Map.tryFind sheet
        |> Option.map (List.filter isRightComponent)
        |> Option.map (
            function 
            | [c] -> Ok(UintType) 
            | [] -> Error($"the ID {id} does not exist") 
            | _ -> failwithf "there are one or more components that match this description (should not happen, dev error not user error)")
        |> Option.defaultWith (failwith $"Component {label} could not be found on Sheet {sheet}")
        *)

/// check that the verification AST obeys the type system 
let rec checkAST (tree: ExprInfo) (sheetComps: Map<string, Component list>): CheckRes = 
    /// Create one error from errors retunred by the evaluation of two operands 
    let propagateError (leftRes: CheckRes) (rightRes: CheckRes) =
        let toErr =
            function
            | TypeInfo _ -> []
            | ErrLst e -> e

        toErr leftRes @ toErr rightRes
    
    /// creates an error caused by argument types being not compatible in the same function
    let hetTypesErr () =
        "This function can't be applied on a bool variable and a non-bool variable"

    /// Creates error caused by function being applied to argument of unsupported type
    let invTypesErr () = "Types not supported by this function"
    
    /// Create a type error
    let makeTypeError errType leftType (rightType: Option<AssertionTypes.Type>) pos =
        let msg = errType () + ". left expr is of type: " + string leftType + if rightType.IsNone  then "" else (". Right expr is of type: " + string (Option.defaultValue UintType rightType))
        ErrLst [ { Msg = msg; Pos = pos; ExtraErrors = None } ]

    /// Check that cast function is applied appropriately
    let checkCast (castExpr: ExprInfo) castType (castSize: Option<int>) pos = 
        let exprRes = checkAST castExpr sheetComps
        match exprRes with 
        | TypeInfo BoolType when castSize.IsSome -> 
            makeTypeError invTypesErr BoolType None pos
        | TypeInfo t -> 
            TypeInfo (Option.defaultValue t castType)
        | _ -> exprRes //propagate error


    /// Check adherence of binary expressions to the type system
    let checkBin l r pos supportsBool makesBool =
        let leftChecked = checkAST l sheetComps
        let rightChecked= checkAST r sheetComps  
        match leftChecked, rightChecked, supportsBool, makesBool with
        | TypeInfo _, TypeInfo _, _, true -> TypeInfo BoolType
        | TypeInfo BoolType, TypeInfo BoolType, true, false -> leftChecked
        | TypeInfo BoolType, TypeInfo BoolType, false, _ -> makeTypeError invTypesErr BoolType (Some BoolType) pos
        | TypeInfo leftT,TypeInfo rightT, _, _ when leftT = BoolType || rightT = BoolType -> makeTypeError hetTypesErr leftT (Some rightT) pos //not same type error
        | TypeInfo _, TypeInfo _, _, _ -> leftChecked
        | _ ->  ErrLst (propagateError leftChecked rightChecked)

    match tree with
    | IsUnary op -> 
        let opRes = checkAST op sheetComps
        match opRes with 
        | TypeInfo _ -> opRes 
        | _ -> failwithf "should not happen" 
    | RequiresBool (l, r, pos) -> // check that operand(s) are bool 
        let leftRes = checkAST l sheetComps
        let rightRes = checkAST r sheetComps 
        match leftRes, rightRes with 
        | TypeInfo typeL, TypeInfo typeR -> 
            if typeL = BoolType && typeL = typeR 
            then leftRes // it's not important what is passed, it's enough to pass type and size information 
            else makeTypeError invTypesErr typeL (Some typeR) pos
        | _ -> ErrLst (propagateError leftRes rightRes)
    | IsBoolExpr (l, r, pos) -> 
        printfn "is bool expr: %A" tree 
        checkBin l r pos true true
    | IsBinExpr (l, r, pos) -> 
        printfn "ledt: %A right %A" l r
        checkBin l r pos false false
    | Lit lit, pos -> 
        match checkLitExistance sheetComps lit with 
            | Ok(litType) -> TypeInfo litType
            | Error(msg) -> ErrLst [ { Msg = msg; Pos = pos; ExtraErrors = None } ]
    | Cast cast, pos -> 
        match cast with
        | ToSigned expr -> checkCast expr (Some IntType) None pos
        | ToUnsigned expr -> checkCast expr (Some UintType) None pos
        | ToBool expr-> checkCast expr (Some BoolType) None pos
        | ToFloat expr -> checkCast expr (Some FloatType) None pos
    | BusCast(newSize, expr), pos -> checkCast expr None (Some newSize) pos
    | _ -> failwithf $"should not happen {tree}" // check that operands (do nothing for unary operators) are the same and that their size is the same
