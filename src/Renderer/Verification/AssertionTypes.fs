module AssertionTypes

type ReplaceType =
    |IODeclaration
    |Assignment
    |Variable of string
    |NoReplace

/// Position of token in the input string
/// Used to localise error messages
type CodePos = {
    Line : int
    Col : int
    Length : int
    CompId: string
}

type CodeExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}

// authored by ln220
/// Error returned by compiler
/// Used to display errors directly in the text editor
type CodeError = {
    Msg: string; 
    Pos: CodePos
    ExtraErrors : CodeExtraErrorInfo array option
}

// this type was created to wrap the results of the evaluate function, otherwise 
// it would have tried to return different types causing errors 
// authored by ln220
/// Wraps possible results that evaluation can return 
type Value = 
    | Int of int64
    | Bool of bool 
    | Uint of uint64
    /// wraps a uint64, the actual conversion happens during the evaluation of the operation
    | Float of uint64 // the conversion only happens at time of evaluation, for the rest floats travel around as uints 

/// Type containing all the necessary information evaluate
/// an identifier in the simulation
type Id = {
    Name: string
    PortNumber: int
    ConnId: string
}

// authored by ln220
/// Wraps two smallest blocks of an expression
/// relevant only for Text assertions as block assertions don't have value tokens
type Lit = 
    | Value of Value
    | Id of Id

// Pattern to make matches on Ids more ergonomic
let (|Id|_|) expr =
    match expr with
    | Id idVal -> Some (idVal.Name, idVal.PortNumber, idVal.ConnId)
    | _ -> None

let makeId name portNumber connId =
    Id {Name = name; PortNumber = portNumber; ConnId = connId}

// authored by ln220
/// Different kinds of type casting supported
type Cast = 
    | ToSigned of ExprInfo
    | ToUnsigned of ExprInfo 
    | ToBool of ExprInfo 
    | ToFloat of ExprInfo

// authored by ln220
/// outermost level for AST 
and Expr = 
    | BoolExpr of BoolExpr 
    | Add of Op 
    | Sub of Op 
    | Mul of Op 
    | Div of Op 
    | Rem of Op 
    | BitOr of Op 
    | BitNot of Op
    | BitAnd of Op 
    | Lit of Lit
    | Cast of Cast 
    | BusCast of int * ExprInfo 

// authored by ln220
/// Expressions that retunr a boolean
and BoolExpr = 
    | Eq of Op//eval 
    | Neq of Op//eval 
    | LogAnd of Op//eval, check
    | LogNot of Op //eval, check
    | LogOr of Op//eval, check
    | Lt of Op//eval 
    | Gt of Op//eval 
    | Gte of Op//eval 
    | Lte of Op//eval

// authored by ln220
/// Operand, differentiates between unary and binary operations
and Op = 
    | BinOp of left: ExprInfo * right: ExprInfo
    | UnOp of ExprInfo

// authored by ln220
/// Attaches position information to an expression to make it easy to know what exactly gave an error
and ExprInfo = Expr * CodePos

// authored by ln220
/// Bus width
and Size = Size of int

// authored by ln220
/// Type of Value
type Type = 
    | IntType
    | UintType 
    | BoolType 
    | FloatType



// TODO(jlsand): Might make sense to make this a result type, so that Result.bind can be used on it.
// authored by ln220
/// Returned by the compiler
type CheckRes = 
    | ErrLst of CodeError list 
    | TypeInfo of Type

// TODO(jsand): This type is a (somewhat) duplicate of Expr.
// However, there is not neccesarily a nice way of unifying the two types.
// For one, Expr contains AST information which tokens inherently do not.
// Additionally, some tokens such as LParen should not be a part of the Expr
// at all. 
type TokenType = 
    | TLit of Lit
    | TAdd              // +
    | TSub              // -
    | TMul              // *
    | TDiv              // /
    | TRem              // %
    | TBitAnd           // &
    | TBitNot           // ~
    | TBitOr            // |
    | TEq               // ==
    | TNeq              // !=
    | TLt               // <
    | TGt               // >
    | TGte              // >=
    | TLte              // <=
    | TLogAnd           // &&
    | TLogNot           // ! 
    | TLogOr            // ||
    | TLParen           // (
    | TRParen           // )
    | TSigned           // signed
    | TUnsigned         // unsigned
    | TBool             // bool
    | TAssertTrue       // assertTrue
    | TAssertFalse      // assertFalse
    | TBusCast of int   // '
    | TInput            // input
    | TComma            // ,
    | TSemicolon        // ;

let tokenSymbol tok =
    match tok with
    | TLit lit          -> sprintf "%A" lit
    | TAdd              -> "+"
    | TSub              -> "-"
    | TMul              -> "*"
    | TDiv              -> "/"
    | TRem              -> "%" 
    | TBitAnd           -> "&"
    | TBitNot           -> "~"
    | TBitOr            -> "|"
    | TEq               -> "=="
    | TNeq              -> "!="
    | TLt               -> "<"
    | TGt               -> ">"
    | TGte              -> ">="
    | TLte              -> "<="
    | TLogAnd           -> "&&"
    | TLogNot           -> "!"
    | TLogOr            -> "||"
    | TLParen           -> "("
    | TRParen           -> ")"
    | TSigned           -> "signed"
    | TUnsigned         -> "unsigned"
    | TBool             -> "bool"
    | TAssertTrue       -> "assertTrue"
    | TAssertFalse      -> "assertFalse"
    | TBusCast width    -> sprintf "%A'" width 
    | TInput            -> "input"
    | TComma            -> ","
    | TSemicolon        -> ";"

type Token = {
    Type: TokenType
    Pos: CodePos
}

type TokenStream = {
    CurToken: Token 
    RemainingTokens: Token list
}

type ParsedInputs = {
    InputNames: string Set
    Stream: TokenStream
}

type ParsedExpr = {
    Expr: Expr
    Stream : TokenStream
}

type ParseExprResult = Result<ParsedExpr, CodeError>

type Precedence = Precedence of int option 

type Assertion = {
    InputNames: string Set;
    AssertExpr: ExprInfo;
    Name: string Option;
    Id: string Option;
    Sheet: string Option;
    Description: string Option
}
