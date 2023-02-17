module PrintAssertions 
open AssertionTypes


let rec print(tree: Expr) : string=
    match tree with
    | Lit (lit, pos) ->

        match lit with
        | Value value ->

            match value with
            | Int int -> string int  
            | Uint uint -> string uint
            | Bool bool -> string bool  

        | Id id -> string 0 // not implemented yet

    | Cast (c, pos) ->

        match c with
        | ToSigned e -> "int(" + (print e) + ")"
        | ToUnsigned e -> "uint" + (print e) + ")" 
        | ToBool e -> "bool" + (print e) + ")" 

    | Add (BinOp(l, r), pos) -> "(" + print l + "+" + (print r) + ")"

    | Sub(BinOp(l, r), pos) -> "(" + print l + "-" + (print r) + ")"

    | Mul(BinOp(l, r), pos) -> "(" + print l + "*" + (print r) + ")"

    | Div(BinOp(l, r), pos) -> "(" + print l + "/" + (print r) + ")"

    | Rem(BinOp(l, r), pos) -> "(" + print l + "%" + (print r) + ")"

    | BitAnd(BinOp(l, r), pos) -> "(" + print l + "&&&" + (print r) + ")"

    | BitOr(BinOp(l, r), pos) -> "(" + print l + "|||" + (print r) + ")"

    | BitNot(UnOp(op), pos) -> "(~~~"+ (print op) + ")"

    | BoolExpr (boolExpr, pos)-> 
        match boolExpr with
        | Neq(BinOp(l, r)) -> "(" + (print l) + "<>" + (print r) + ")"
        | LogAnd(BinOp(l, r)) -> "(" + (print l) + "&&" + (print r) + ")"
        | LogOr(BinOp(l, r)) -> "(" + (print l) + "||" + (print r) + ")"
        | Lt(BinOp(l, r)) -> "(" + (print l) + "<" + (print r) + ")"
        | Gt(BinOp(l, r)) -> "(" + (print l) + ">" + (print r) + ")"
        | Gte(BinOp(l, r)) -> "(" + (print l) + ">=" + (print r) + ")"
        | Lte(BinOp(l, r)) -> "(" + (print l) + "<=" + (print r) + ")"
        | LogNot(UnOp op) -> "(not"+ (print op) + ")"
        | _ -> failwithf "not implemented yet"

    | _ -> string 0
