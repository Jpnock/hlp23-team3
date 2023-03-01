module AssertionCheck

open AssertionTypes
open CommonTypes
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

let getLitMinSize lit = 
    let num = 
        match lit with
        | Int int -> float int 
        | Uint uint -> float uint 
        | Bool bool -> 1. //technically not needed but will be returned, otherwise can put size as an option bt a bit of a pain
    let log = Math.Log(num, 2.)
    (ceil >> int) log

let getType value = 
    match value with 
    | Int _ -> IntType
    | Uint _ -> UintType 
    | Bool _ -> BoolType 

let getLitProperties (components: Component List) lit = 
    match lit with
    | Value value -> getType value, getLitMinSize value 
    | Id id -> 
        let width = 
            let isRightComponent (comp: Component) = 
                match comp.Label, comp.Type with 
                | idComp, Viewer width when idComp = id -> Some(width)
                | idComp, Input1 (width,_) when idComp = id -> Some(width)
                | _ -> None 
            List.choose isRightComponent components 
            |> List.head
        UintType, int width


// for unary expressions type checks are not needed, it's enough to return the type of the variable (tricky because it needs to take into account casts)
// maybe do an active matching to check if it's a unary operator, in that case only evaluate the type and size
let rec checkAST (tree: ExprInfo) (components: Component List): CheckRes = 
    let propagateError (leftRes: CheckRes) (rightRes: CheckRes) =
        let toErr =
            function
            | Properties _ -> []
            | ErrLst e -> e

        toErr leftRes @ toErr rightRes
    
    let hetTypesErr () =
        "This function can't be applied on value of different types"

    let invTypesErr () = "Types not supported by this function"
    
    let makeTypeError errType leftType (rightType: Option<AssertionTypes.Type>) pos =
        let msg = errType () + ". left expr is of type: " + string leftType + if rightType.IsNone  then "" else (". Right expr is of type: " + string (Option.defaultValue UintType rightType))
        ErrLst [ { Msg = msg; Pos = pos } ]

    let makeSizeError leftSize rightSize pos = 
        let msg = "The buses have different widths. Left expr is of size: " + string leftSize + if rightSize = 0 then "" else (". Right expr is of size: " + string rightSize)
        ErrLst [ { Msg = msg; Pos = pos } ]
        

    let checkCast (castExpr: ExprInfo) castType (castSize: Option<int>) pos = 
        let exprRes = checkAST castExpr components
        match exprRes with 
        | Properties {Type = BoolType; Size = _} when castSize.IsSome -> 
            makeTypeError invTypesErr BoolType None pos
        | Properties {Type = t ; Size = size} -> 
            Properties {Type = Option.defaultValue t castType; Size = Option.defaultValue size castSize} 
        | _ -> exprRes //propagate error

    let checkSize exprL exprR propertiesL propertiesR pos = 
        let checkLit sizeLit sizeOther typeOther left= 
            if sizeLit <= sizeOther 
            then Properties{Type = typeOther; Size = sizeOther}
            else 
                if left
                then makeSizeError sizeLit sizeOther pos//make this better (with a general function that also prints the sizes on the left and on the right)
                else makeSizeError sizeOther sizeLit pos
    
        match propertiesL, propertiesR with 
        | Properties {Type = typeL; Size = sizeL}, Properties {Type = typeR; Size = sizeR} -> 
            match exprL, exprR with 
            | (Lit litL, _), (Lit litR, _) -> 
                printfn "both lit"
                Properties{Type = typeL; Size = max sizeL sizeR}
            | (Lit litL, _), _ -> 
                printfn "one lit"
                printfn "%A" exprR
                printfn "%A"  exprL
                checkLit sizeL sizeR typeR true
            | _, (Lit litR, _) -> 
                printfn "one lit, the right one"
                printfn "%A" exprL
                printfn "%A"  exprR
                checkLit sizeR sizeL typeL false
            | _, _ -> 
                printfn "creating error potentially"
                if sizeL = sizeR then propertiesL else makeSizeError sizeL sizeR pos
        | _ -> ErrLst (propagateError propertiesL propertiesR)

    let checkBin l r pos supportsBool =
        let leftChecked = checkAST l components
        let rightChecked= checkAST r components  
        match leftChecked, rightChecked with
        | Properties {Type = typeL; Size = sizeL}, Properties {Type = typeR; Size = sizeR} -> 
            if typeL = typeR && typeL = BoolType
            then
                if supportsBool 
                then leftChecked
                else makeTypeError invTypesErr typeL (Some typeR) pos
            elif typeL = typeR 
            then checkSize l r leftChecked rightChecked pos
            else makeTypeError hetTypesErr typeL (Some typeR) pos //not same type error
        | _ ->  ErrLst (propagateError leftChecked rightChecked)

    match tree with
    | IsUnary op -> 
        let opRes = checkAST op components
        match opRes with 
        | Properties _ -> opRes 
        | _ -> failwithf "should not happen" 
    | RequiresBool (l, r, pos) -> // check that operand(s) are bool 
        let leftRes = checkAST l components
        let rightRes = checkAST r components 
        match leftRes, rightRes with 
        | Properties {Type = typeL; Size = _}, Properties {Type = typeR; Size =_} -> 
            if typeL = BoolType && typeL = typeR 
            then leftRes // it's not important what is passed, it's enough to pass type and size information 
            else makeTypeError invTypesErr typeL (Some typeR) pos
        | _ -> ErrLst (propagateError leftRes rightRes)
    | IsBoolExpr (l, r, pos) -> checkBin l r pos true
    | IsBinExpr (l, r, pos) -> checkBin l r pos false
    | Lit lit, _ -> 
        let litType, size = getLitProperties components lit
        Properties {Type = litType; Size = size}
    | Cast cast, pos -> 
        match cast with
        | ToSigned expr -> checkCast expr (Some IntType) None pos
        | ToUnsigned expr -> checkCast expr (Some UintType) None pos
        | ToBool expr-> checkCast expr (Some BoolType) None pos
    | BusCast(newSize, expr), pos -> checkCast expr None (Some newSize) pos
    | _ -> failwithf "should not happen" // check that operands (do nothing for unary operators) are the same and that their size is the same
