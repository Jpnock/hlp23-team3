/// Author: James Nock
module AssertionASTMap

open AssertionTypes

type InputPortNumber = int
type OutputPortNumber = int
type PortExprs = Map<InputPortNumber, Expr>
type ASTBuilder = PortExprs -> Expr option

let noAssertion _ =
    None

let getExprInfo (exprs : PortExprs) port : ExprInfo =
    exprs[port], {Length = 0; Line = 0; Col = 0}

let binaryOp (exprs : PortExprs) =
   (getExprInfo exprs 0, getExprInfo exprs 1)
   |> BinOp

let astMapper (typ : TokenType) (exprs : PortExprs) : Expr option =
    match typ with
    | TLit _ ->
        printf "Literal not yet implemented"
        None
    | TAdd -> Add (binaryOp exprs) |> Some
    | TSub -> Sub (binaryOp exprs) |> Some   
    | TMul -> Mul (binaryOp exprs) |> Some
    | TDiv -> Div (binaryOp exprs) |> Some
    | TRem -> Rem (binaryOp exprs) |> Some
    | TBitAnd -> BitAnd (binaryOp exprs) |> Some
    | TBitNot -> BitNot (binaryOp exprs) |> Some 
    | TBitOr -> BitOr (binaryOp exprs) |> Some
    | TEq -> BoolExpr (Eq (binaryOp exprs)) |> Some 
    | TNeq -> BoolExpr (Neq (binaryOp exprs)) |> Some
    | TLt -> BoolExpr (Lt (binaryOp exprs)) |> Some  
    | TGt -> BoolExpr (Gt (binaryOp exprs)) |> Some  
    | TGte -> BoolExpr (Gte (binaryOp exprs)) |> Some  
    | TLte -> BoolExpr (Lte (binaryOp exprs)) |> Some  
    | TLogAnd -> BoolExpr (LogAnd (binaryOp exprs)) |> Some  
    | TLogNot -> BoolExpr (LogNot (binaryOp exprs)) |> Some  
    | TLogOr -> BoolExpr (LogOr (binaryOp exprs)) |> Some  
    | TLParen -> None 
    | TRParen -> None
    | TSigned ->
        printf "Signed not yet implemented"
        None
    | TUnsigned ->
        printf "Unsigned not yet implemented"
        None
    | TBool -> None
    | TBusCast _->
        printf "Bus cast not yet implemented"
        None
