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
type ASTBuilder = string -> PortExprs -> Expr option

let private getExprInfo (exprs : PortExprs) port compId: ExprInfo =
    exprs[port], {Length = 0; Line = 0; Col = 0; CompId = compId}

let private binaryOp (exprs : PortExprs) compId =
   (getExprInfo exprs 0 compId, getExprInfo exprs 1 compId)
   |> BinOp

let noAssertion _ _=
    None

/// Helper function which can be partially applied to return an ASTBuilder.
/// The `typ` argument represents what type of component should be built;
/// for example, TAdd will construct an `Add` AST based on the provided
/// input port expressions.
let astMapper (typ : TokenType) (compId: string)  (exprs : PortExprs) : Expr option =
    match typ with
    | TLit _ ->
        printf "Literal not yet implemented"
        None
    | TAdd -> Add (binaryOp exprs compId) |> Some
    | TSub -> Sub (binaryOp exprs compId) |> Some   
    | TMul -> Mul (binaryOp exprs compId) |> Some
    | TDiv -> Div (binaryOp exprs compId) |> Some
    | TRem -> Rem (binaryOp exprs compId) |> Some
    | TBitAnd -> BitAnd (binaryOp exprs compId) |> Some
    | TBitNot -> BitNot (binaryOp exprs compId) |> Some 
    | TBitOr -> BitOr (binaryOp exprs compId) |> Some
    | TEq -> BoolExpr (Eq (binaryOp exprs compId)) |> Some 
    | TNeq -> BoolExpr (Neq (binaryOp exprs compId)) |> Some
    | TLt -> BoolExpr (Lt (binaryOp exprs compId)) |> Some  
    | TGt -> BoolExpr (Gt (binaryOp exprs compId)) |> Some  
    | TGte -> BoolExpr (Gte (binaryOp exprs compId)) |> Some  
    | TLte -> BoolExpr (Lte (binaryOp exprs compId)) |> Some  
    | TLogAnd -> BoolExpr (LogAnd (binaryOp exprs compId)) |> Some  
    | TLogNot -> BoolExpr (LogNot (binaryOp exprs compId)) |> Some  
    | TLogOr -> BoolExpr (LogOr (binaryOp exprs compId)) |> Some  
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
