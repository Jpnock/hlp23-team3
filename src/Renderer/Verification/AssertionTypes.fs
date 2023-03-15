module AssertionTypes

type ReplaceType =
    |IODeclaration
    |Assignment
    |Variable of string
    |NoReplace


type CodePos = {
    Line : int
    Col : int
    Length : int
}

type CodeExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}

type CodeError = {
    Msg: string; 
    Pos: CodePos
    ExtraErrors : CodeExtraErrorInfo array option
}

// this type was created to wrap the results of the evaluate function, otherwise 
// it would have tried to return different types causing errors 
// authored by ln220
type Value = 
    | Int of int 
    | Bool of bool 
    | Uint of uint 

// authored by ln220
type Lit = 
    | Value of Value
    //| Id of string // for now, later make it of BusLabel of whatever type there is 
    | Id of (string * int)// for now, later make it of BusLabel of whatever type there is 

// authored by ln220
type Cast = 
    | ToSigned of ExprInfo
    | ToUnsigned of ExprInfo 
    | ToBool of ExprInfo 

// authored by ln220
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
and Op = 
    | BinOp of left: ExprInfo * right: ExprInfo
    | UnOp of ExprInfo

// authored by ln220
and ExprInfo = Expr * CodePos 

// authored by ln220
and Size = Size of int

// authored by ln220
type Type = 
    | IntType
    | UintType 
    | BoolType 

// authored by ln220
type Properties = {
    Type: Type;
    Size: int 
}


// TODO(jlsand): Might make sense to make this a result type, so that Result.bind can be used on it.
// authored by ln220
type CheckRes = 
    | ErrLst of CodeError list 
    | Properties of Properties 

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

type ParseData = {
    Expr: Expr
    RemainingTokens : Token list
}

type ParseResult = Result<ParseData, CodeError>

type Precedence = Precedence of int option 

//type ExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}
//type ErrorInfo = {Line:int; Col:int; Length: int; Message: string; ExtraErrors: ExtraErrorInfo array option}

type Assertion = {
    AST: ExprInfo;
}
