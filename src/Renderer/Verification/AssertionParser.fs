(*
    AssertionParser.fs

    Hand-written parser for assertion logic.
*)

module AssertionParser

open VerilogTypes
open EEExtensions
open AssertionTypes

type LexResult = {
    Token: Token
    RemainingCode: string
}

let createRegMatch regex str =
    let regex' = "^" + regex 
    String.regexMatchFull regex' str


let (|NewlineRegex|_|) =
    createRegMatch "\n"

let (|WhitespaceRegex|_|) =
    createRegMatch "( |\t)+"

let (|IntegerRegex|_|) str =
    createRegMatch "^[1-9]\d*" str

let (|AddRegex|_|) =
    createRegMatch "\+"

let (|SubRegex|_|) =
    createRegMatch "-"

let (|MulRegex|_|) =
    createRegMatch "\*"

let (|DivRegex|_|) =
    createRegMatch "/"

let (|RemRegex|_|) =
    createRegMatch "%"

let (|BitAndRegex|_|) =
    createRegMatch "&"

let (|BitNotRegex|_|) =
    createRegMatch "~"

let (|BitOrRegex|_|) =
    createRegMatch "\|"

let (|EqRegex|_|) =
    createRegMatch "=="

let (|NeqRegex|_|) =
    createRegMatch "!="

let (|LtRegex|_|) =
    createRegMatch "<"

let (|GtRegex|_|) =
    createRegMatch ">"

let (|GteRegex|_|) =
    createRegMatch ">="

let (|LteRegex|_|) =
    createRegMatch "<="

let (|LogAndRegex|_|) =
    createRegMatch "&&"

let (|LogNotRegex|_|) =
    createRegMatch "!"

let (|LogOrRegex|_|) =
    createRegMatch "\|\|"
    
let (|InvalidRegex|_|) =
    createRegMatch ".*(\n|$)"


let rec lexAssertion (code: string) curLine curCol (tokens: Token list): Result<Token list, Error> =
    if code.Length = 0 then
        Ok tokens
    else
        let addToken tokType len= 
            let remainingCode = code.Substring len
            let token = {Type = tokType; Pos = {Line = curLine; Col = curCol; Length = len}}
            let tokens' = List.append tokens [token]
            lexAssertion remainingCode curLine (curCol + len) tokens'

        match code with 
        | NewlineRegex m ->     lexAssertion (code.Substring m.Length) (curLine+1) 0 tokens
        | WhitespaceRegex m ->  lexAssertion (code.Substring m.Length) curLine (curCol + m.Length) tokens
        | IntegerRegex m ->
            // TODO: Kind of confusing syntax
            let t = System.Int32.Parse m.Value |> Int |> Value |> Lit 
            addToken t m.Length
        | AddRegex m ->         addToken Add m.Length
        | SubRegex m ->         addToken Sub m.Length
        | MulRegex m ->         addToken Mul m.Length
        | DivRegex m ->         addToken Div m.Length
        | RemRegex m ->         addToken Rem m.Length
        | LogAndRegex m ->      addToken LogAnd m.Length
        | LogNotRegex m ->      addToken LogNot m.Length
        | LogOrRegex m ->       addToken LogOr m.Length
        | BitAndRegex m ->      addToken BitAnd m.Length
        | BitNotRegex m ->      addToken BitNot m.Length
        | BitOrRegex m ->       addToken BitOr m.Length
        | EqRegex m ->          addToken Eq m.Length
        | NeqRegex m ->         addToken Neq m.Length
        | GteRegex m ->         addToken Gte m.Length
        | LteRegex m ->         addToken Lte m.Length
        | LtRegex m ->          addToken Lt m.Length
        | GtRegex m ->          addToken Gt m.Length
        | InvalidRegex m ->
            Error {
                Pos = {Line = curLine; Col = curCol; Length = m.Length}; 
                Msg = "Unrecognized token: " + m.Value; 
            }
        | _ ->
            failwithf "What? Should not be possible for code not to be matched to token in assertion lexer"


// TODO: Add DU for precedence
// See https://class.ece.uw.edu/cadta/verilog/operators.html
let operatorPrecedence (tokType:TokenType) =
    match tokType with
    | TokenType.LogNot -> 100
    | Mul | Div | Rem -> 80
    | Add | Sub -> 70
    | Gt | Gte | Lt | Lte -> 60
    | Eq | Neq -> 50
    | BitAnd -> 40
    | BitOr -> 30
    | LogAnd -> 20
    | LogOr -> 10
    | _ -> 0

let rec parseOperand tokens : ParseResult =
    let token = List.head tokens
    let remTokens = List.tail tokens
    match token.Type with
    | Lit l -> 
        let expr = Expr.Lit (l, token.Pos)
        Ok {Expr = expr ; RemainingTokens = remTokens}
    | _ ->
        Error {Msg = sprintf "%A is not a valid operand!" token.Type; Pos = token.Pos }

    
let rec parseBinaryOp minPrecedence (lhs:ParseData) :ParseResult =

    if List.isEmpty lhs.RemainingTokens then
        // No more tokens 
        Ok lhs
    else 
        let peekToken = List.head lhs.RemainingTokens

        let precedence = operatorPrecedence peekToken.Type
        if precedence = 0 || (precedence < minPrecedence) then
            // Peek token does not have a high enough precedence or 
            // is not a binary operator at all.
            Ok lhs
        else
            // Token is valid binary operator and has high enough precedence to continue
            let opToken = peekToken

            if List.length lhs.RemainingTokens = 1 then
                // Operator is last token in token stream. No RHS operand to operate on!
                Error { Msg = "Unexpected end of assertion code."; Pos = opToken.Pos}
            else 
                let rhs = parseExpr (precedence + 1) <| List.tail lhs.RemainingTokens

                rhs
                |> Result.bind (fun rhsData ->
                    let binaryOperands = BinOp (lhs.Expr, rhsData.Expr)
                    let exprData = (binaryOperands, opToken.Pos) 

                    let binaryExpr = 
                        match opToken.Type with
                        | TokenType.Add ->
                            Expr.Add exprData
                        | TokenType.Sub ->
                            Expr.Sub exprData
                        | TokenType.Mul ->
                            Expr.Mul exprData
                        | TokenType.Div ->
                            Expr.Div exprData
                        | TokenType.Rem ->
                            Expr.Rem exprData
                        | TokenType.BitAnd ->
                            Expr.BitAnd exprData
                        | TokenType.BitNot ->
                            Expr.BitNot exprData
                        | TokenType.BitOr ->
                            Expr.BitOr exprData
                        | TokenType.Eq ->
                            Expr.BoolExpr (BoolExpr.Eq binaryOperands, opToken.Pos) 
                        | TokenType.Neq ->
                            Expr.BoolExpr (BoolExpr.Neq binaryOperands, opToken.Pos) 
                        | TokenType.LogAnd ->
                            Expr.BoolExpr (BoolExpr.LogAnd binaryOperands, opToken.Pos) 
                        | TokenType.LogNot ->
                            Expr.BoolExpr (BoolExpr.LogNot binaryOperands, opToken.Pos) 
                        | TokenType.LogOr ->
                            Expr.BoolExpr (BoolExpr.LogOr binaryOperands, opToken.Pos) 
                        | TokenType.Lt ->
                            Expr.BoolExpr (BoolExpr.Lt binaryOperands, opToken.Pos) 
                        | TokenType.Gt ->
                            Expr.BoolExpr (BoolExpr.Gt binaryOperands, opToken.Pos) 
                        | TokenType.Gte ->
                            Expr.BoolExpr (BoolExpr.Gte binaryOperands, opToken.Pos) 
                        | TokenType.Lte ->
                            Expr.BoolExpr (BoolExpr.Lte binaryOperands, opToken.Pos) 
                        | TokenType.Lit _ -> 
                            failwithf "What? Should not be possible for a literal to be treated as a binary operand."
                    
                    parseBinaryOp minPrecedence <| {Expr = binaryExpr; RemainingTokens = rhsData.RemainingTokens}
                )

and parseExpr minPrecedence (tokens: Token list):ParseResult =
    parseOperand tokens 
    |> Result.bind (parseBinaryOp minPrecedence)


let parseAssertion code: Result<Expr, ErrorInfo>=
    printfn "Parsing string: %A" code

    lexAssertion code 0 0 []
    |> Result.bind (fun tokens ->
        parseExpr 0 <| List.rev tokens
    )
    |> Result.map (fun parseData -> parseData.Expr)
    |> Result.mapError (fun e -> {Message = e.Msg; Line = e.Pos.Line; Col = e.Pos.Col; Length = e.Pos.Length; ExtraErrors = None})

    






(*
Literal Tokens:
Integer, 21, -3
Bool e.g. true, false
Id e.g busA


Expr Tokens:
Add + 
Sub -
Mul *
Div /
Rem %
BitAnd &
BitNot ~
BitOr |
Lit of Lit * Pos//eval
Cast of Cast * Pos//eval
BusCast of uint * Expr * Pos//TODO


BoolExpr Tokens:
Eq ==
Neq !=
Lt <
Gt >
Gte >=
Lte <=
LogAnd &&
LogNot ! 
LogOr ||





*)
