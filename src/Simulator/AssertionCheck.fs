module AssertionCheck

open AssertionTypes
open AssertionEvaluation
open CommonTypes
open System
let (|RequiresBool|_|) (expr: Expr) = 
    match expr with 
    | BoolExpr(LogAnd(BinOp(l, r)), pos) | BoolExpr(LogOr (BinOp(l, r)), pos) -> Some(l, r, pos)
    | _ -> None 

// no checks needed, return the type of the expression
let (|IsUnary|_|) (expr: Expr) =
    match expr with 
    | BitNot(UnOp(op), _) | BoolExpr(LogNot(UnOp(op)), _) -> Some(op)
    | _ -> None

// this is useful as it allows to neatly extract the expressions from the wrappers and check those directly 
// without having to match and repeat code in the main function 
let (|IsBoolExpr|_| ) (expr: Expr) = 
    match expr with 
    | BoolExpr(boolExpr, pos) ->
        match boolExpr with 
        | Eq(BinOp(l, r)) | Neq(BinOp(l, r)) | Lt(BinOp(l, r)) | Gt(BinOp(l, r)) | Lte(BinOp(l,r)) | Gte(BinOp(l,r)) -> Some(l, r, pos)
        | _ -> None 
    | _ -> None

let (|IsBinExpr|_|) (expr: Expr) = 
    match expr with 
    | Add(BinOp(l, r), pos) | Sub(BinOp(l, r), pos) | Mul(BinOp(l, r), pos) | Div(BinOp(l, r), pos)| Rem(BinOp(l, r), pos) -> Some(l, r, pos)
    | _ -> None

let getLitMinWidth lit = 
    let num = 
        match lit with
        | Int int -> float int 
        | Uint uint -> float uint 
        | Bool bool -> 1. //technically not needed but will be returned, otherwise can put Width as an option bt a bit of a pain
    let log = Math.Log(num, 2.)
    (ceil >> int) log

let getType value = 
    match value with 
    | Int _ -> IntType
    | Uint _ -> UintType 
    | Bool _ -> BoolType 
// for unary expressions type checks are not needed, it's enough to return the type of the variable (tricky because it needs to take into account casts)
// maybe do an active matching to check if it's a unary operator, in that case only evaluate the type and Width
let rec checkAST (tree: Expr) (components: Component List): CheckRes = 
    let propagateError (leftRes: CheckRes) (rightRes: CheckRes) =
        let toErr =
            function
            | ValInfo _ -> []
            | ErrLst e -> e

        toErr leftRes @ toErr rightRes
    
    let hetTypesErr () =
        "This function can't be applied on value of different types"

    let invTypesErr () = "Types not supported by this function"
    
    let makeTypeError errType leftType (rightType: Option<AssertionTypes.Type>) pos =
        let msg = errType () + ". left expr is of type: " + string leftType + if rightType.IsNone  then "" else (". Right expr is of type: " + string rightType)
        ErrLst [ { Msg = msg; Pos = pos } ]

    let makeWidthError leftWidth  rightWidth pos = 
        let msg = "The buses have different widths. Left expr is of Width: " + string leftWidth + if rightWidth = 0 then "" else (". Right expr is of Width: " + string rightWidth)
        ErrLst [ { Msg = msg; Pos = pos } ]
        

    let checkCast castExpr castType castWidth = 
        let exprRes = checkAST castExpr components 
        match exprRes with 
        | ValInfo {Type = t ; Width = width} -> 
            ValInfo {Type = Option.defaultValue t castType; Width = Option.defaultValue width castWidth} 
        | _ -> exprRes //propagate error

    let checkWidth exprL exprR valInfoL valInfoR pos = 
        let checkLit widthLit widthOther typeOther= 
            if widthLit <= widthOther 
            then ValInfo{Type = typeOther; Width = widthOther}
            else makeWidthError widthLit widthOther pos//make this better (with a general function that also prints the Widths on the left and on the right)
    
        match valInfoL, valInfoR with 
        | ValInfo {Type = typeL; Width = widthL}, ValInfo {Type = typeR; Width = widthR} -> 
            match exprL, exprR with 
            | Lit (litL, _), Lit (litR, _) -> ValInfo{Type = typeL; Width = max widthL widthR}
            | Lit (litL, _), _ -> checkLit widthL widthR typeR
            | _, Lit (litR, _) -> checkLit widthR widthL typeL
            | _, _ -> if widthL = widthR then valInfoL else makeWidthError widthL widthR pos
        | _ -> ErrLst (propagateError valInfoL valInfoR)

    let checkBin l r pos supportsBool =
        let leftChecked = checkAST l components
        let rightChecked= checkAST r components 
        match leftChecked, rightChecked with
        | ValInfo {Type = typeL; Width = widthL}, ValInfo {Type = typeR; Width = widthR} -> 
            if typeL = typeR && typeL = BoolType
            then
                if supportsBool 
                then leftChecked
                else makeTypeError invTypesErr typeL (Some typeR) pos
            elif typeL = typeR 
            then checkWidth l r leftChecked rightChecked pos
            else makeTypeError hetTypesErr typeL (Some typeR) pos //not same type error
        | _ ->  ErrLst (propagateError leftChecked rightChecked)

    match tree with
    | IsUnary op -> 
        let opRes = checkAST op components
        match opRes with 
        | ValInfo _ -> opRes 
        | _ -> failwithf "should not happen" 
    | RequiresBool (l, r, pos) -> // check that operand(s) are bool 
        let leftRes = checkAST l components
        let rightRes = checkAST r components
        match leftRes, rightRes with 
        | ValInfo {Type = typeL; Width = _}, ValInfo {Type = typeR; Width =_} -> 
            if typeL = BoolType && typeL = typeR 
            then leftRes // it's not important what is passed, it's enough to pass type and Width information 
            else makeTypeError invTypesErr typeL (Some typeR) pos
        | _ -> ErrLst (propagateError leftRes rightRes)
    | IsBoolExpr (l, r, pos) -> checkBin l r pos true
    | IsBinExpr (l, r, pos) -> checkBin l r pos false
    | Lit(lit, _) -> 
        match lit with
        | Value value -> ValInfo{Type = getType value; Width =  getLitMinWidth value}
        | Id id -> 
            let isRightComponent (comp: Component) = 
                match comp.Label, comp.Type with
                //| id, Viewer width  -> Some width 
                | _ -> None
            //List.choose isRightComponent components 
            //|> List.head // there should only be 1 element in thsi list 
            failwithf "dont' know what the fuck is happening"
    | Cast(cast, _) -> 
        match cast with
        | ToSigned expr -> checkCast expr (Some IntType) None
        | ToUnsigned expr -> checkCast expr (Some UintType) None
        | ToBool expr-> checkCast expr (Some BoolType) None
    | BusCast(newWidth, expr, pos) -> checkCast expr None (Some newWidth)
    | _ -> failwithf "should not happen" // check that operands (do nothing for unary operators) are the same and that their Width is the same