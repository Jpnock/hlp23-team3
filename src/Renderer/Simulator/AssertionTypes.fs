module AssertionTypes

type Pos = {
    Start: uint ;
    End: uint
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


type Type = 
    | IntType
    | UintType 
    | BoolType 

type ValInfo = {
    Type: Type;
    Size: int 
}

type CheckRes = 
    | ErrLst of Error list 
    | ValInfo of ValInfo

