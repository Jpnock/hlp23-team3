/// Contains logic for converting between components
/// and their respective Assertion AST types.
///
/// Authored by jpn119 (James Nock)
module AssertionASTMap

open AssertionTypes

/// Represents the port number (0..N) of an input on a given
/// component.
type InputPortNumber = int

/// Represents the port number (0..N) of an output on a given
/// component.
type OutputPortNumber = int

/// Helper type which represents an Assertion AST Expression for the
/// connection that feeds into a given InputPortNumber of a component.
type PortExprs = Map<InputPortNumber, Expr>

/// Helper type for functions that accept the current map of built Assertion
/// AST Expressions for all inputs of a component; the functions should return
/// a new Expression representing the AST including the component.
type ASTBuilder = PortExprs -> Expr option

let private getExprInfo (exprs : PortExprs) port : ExprInfo =
    exprs[port], {Length = 0; Line = 0; Col = 0}

let private binaryOp (exprs : PortExprs) =
   (getExprInfo exprs 0, getExprInfo exprs 1)
   |> BinOp

let private unaryOp (exprs: PortExprs) =
    printf "trying to create log not %A" exprs
    let expr1 = (getExprInfo exprs 0)
    printf "expr %A" expr1
    expr1 |> UnOp 

let noAssertion _ =
    None

/// Helper function which can be partially applied to return an ASTBuilder.
/// The `typ` argument represents what type of component should be built;
/// for example, TAdd will construct an `Add` AST based on the provided
/// input port expressions.
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
    | TBitNot -> BitNot (unaryOp exprs) |> Some 
    | TBitOr -> BitOr (binaryOp exprs) |> Some
    | TEq -> BoolExpr (Eq (binaryOp exprs)) |> Some 
    | TNeq -> BoolExpr (Neq (binaryOp exprs)) |> Some
    | TLt -> BoolExpr (Lt (binaryOp exprs)) |> Some  
    | TGt -> BoolExpr (Gt (binaryOp exprs)) |> Some  
    | TGte -> BoolExpr (Gte (binaryOp exprs)) |> Some  
    | TLte -> BoolExpr (Lte (binaryOp exprs)) |> Some  
    | TLogAnd -> BoolExpr (LogAnd (binaryOp exprs)) |> Some  
    | TLogNot -> BoolExpr (LogNot (unaryOp exprs)) |> Some  
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
    | TAssertFalse -> None
    | TAssertTrue -> None
    | TComma -> None
    | TInput -> None
    | TSemicolon -> None
