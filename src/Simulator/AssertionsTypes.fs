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
    | BitNot of Op * Pos //eval
    | BitAnd of Op * Pos//eval
    | Lit of Lit * Pos//eval
    | Cast of Cast * Pos//eval
    | BusCast of uint * Expr * Pos//TODO

and BoolExpr = 
    | Eq of Op//eval 
    | Neq of Op//eval 
    | LogAnd of Op//eval 
    | LogNot of Op 
    | LogOr of Op//eval
    | Lt of Op//eval 
    | Gt of Op//eval 
    | Gte of Op//eval 
    | Lte of Op//eval

and Op = 
    | BinOp of left: Expr * right: Expr 
    | UnOp of Expr 

