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
    | ToSigned of Expr 
    | ToUnsigned of Expr 
    | ToBool of Expr 

and Expr = 
    | BoolExpr of BoolExpr * Pos
    | Add of Op * Pos//eval
    | Sub of Op * Pos//eval 
    | Mul of Op * Pos //eval 
    | Div of Op * Pos//eval  
    | Rem of Op * Pos//eval
    | BitOr of Op * Pos//val 
    | BitNot of Op * Pos //eval, check
    | BitAnd of Op * Pos//eval
    | Lit of Lit * Pos//eval check
    | Cast of Cast * Pos//eval check 
    | BusCast of uint * Expr * Pos // check

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
    | BinOp of left: Expr * right: Expr 
    | UnOp of Expr 

and ExprInfo = {
    Op: Op; 
    Pos: Pos; 
}

type Size = Size of uint

type Type = 
    | IntType
    | UintType 
    | BoolType 

type ValInfo = {
    Type: Type;
    Size: Size
}

type CheckRes = 
    | ErrLst of Error list 
    | ValInfo of ValInfo

