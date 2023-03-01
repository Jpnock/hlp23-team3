module AssertionTypes

type Pos = {
    Line : int
    Col : int
    Length : int
}

type Error = {
    Msg: string; 
    Pos: Pos
}

// this type was created to wrap the results of the evaluate function, otherwise 
// it would have tried to return different types causing errors 
type Value = 
    | Int of int 
    | Bool of bool 
    | Uint of uint 

type Lit = 
    | Value of Value
    | Id of string // for now, later make it of BusLabel of whatever type there is 

type Cast = 
    | ToSigned of ExprInfo
    | ToUnsigned of ExprInfo 
    | ToBool of ExprInfo 

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

and Op = 
    | BinOp of left: ExprInfo * right: ExprInfo
    | UnOp of ExprInfo

and ExprInfo = Expr * Pos 

and Size = Size of int

type Type = 
    | IntType
    | UintType 
    | BoolType 

type Properties = {
    Type: Type;
    Size: int 
}

type CheckRes = 
    | ErrLst of Error list 
    | Properties of Properties 

// TODO: Add cast
// TODO(jsand): This type is a (somewhat) duplicate of Expr.
// However, there is not neccesarily a nice way of unifying the two types.
// For one, Expr contains AST information which tokens inherently do not.
// Additionally, some tokens such as LParen should not be a part of the Expr
// at all. 
type TokenType = 
    | TLit of Lit
    | TAdd       // +
    | TSub       // -
    | TMul       // *
    | TDiv       // /
    | TRem       // %
    | TBitAnd    // &
    | TBitNot    // ~
    | TBitOr     // |
    | TEq        // ==
    | TNeq       // !=
    | TLt        // <
    | TGt        // >
    | TGte       // >=
    | TLte       // <=
    | TLogAnd    // &&
    | TLogNot    // ! 
    | TLogOr     // ||
    | TLParen    // (
    | TRParen    // )
    | TSigned    // signed
    | TUnsigned  // unsigned
    | TBool      // bool
    | TBusCast   // '

type Token = {
    Type: TokenType
    Pos: Pos
}

type ParseData = {
    Expr: Expr
    RemainingTokens : Token list
}

type ParseResult = Result<ParseData, Error>

type Precedence = Precedence of int option 

type ReplaceType =
    |IODeclaration
    |Assignment
    |Variable of string
    |NoReplace

type ExtraErrorInfo = {Text: string; Copy: bool; Replace: ReplaceType}

type ErrorInfo = {Line:int; Col:int; Length: int; Message: string; ExtraErrors: ExtraErrorInfo array option}

type Assertion = {
    AST: ExprInfo;
}