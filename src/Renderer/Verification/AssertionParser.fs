(*
    AssertionParser.fs

    Hand-written parser for assertion logic.
*)

module AssertionParser

open EEExtensions
open AssertionTypes

let (|RegexPattern|_|) regex str =
    let regex' = "^" + regex 
    String.regexMatchFull regex' str

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
        | RegexPattern "\n" m -> 
            // Count new lines, but don't add them to the list of tokens
            lexAssertion (code.Substring m.Length) (curLine+1) 0 tokens
        | RegexPattern "( |\t)+" m -> 
            // Similar concept for white space
            lexAssertion (code.Substring m.Length) curLine (curCol + m.Length) tokens
        | RegexPattern "[1-9]\d*" m -> 
            // integer literals
            let intTok = System.Int32.Parse m.Value |> Int |> Value |> TLit 
            addToken intTok m.Length
        | RegexPattern "signed" m -> addToken TSigned m.Length 
        | RegexPattern "unsigned" m -> addToken TUnsigned m.Length 
        | RegexPattern "bool" m -> addToken TBool m.Length 
        | RegexPattern "[_a-zA-Z][_a-zA-Z0-9]*" m ->
            // identifiers
            let idTok = Id m.Value |> TLit
            addToken idTok m.Length
        | RegexPattern "\(" m ->   addToken TLParen m.Length
        | RegexPattern "\)" m ->   addToken TRParen m.Length
        | RegexPattern "\+" m ->   addToken TAdd m.Length
        | RegexPattern "-" m ->    addToken TSub m.Length
        | RegexPattern "\*" m ->   addToken TMul m.Length
        | RegexPattern "/" m ->    addToken TDiv m.Length
        | RegexPattern "%" m ->    addToken TRem m.Length
        | RegexPattern "&&" m ->   addToken TLogAnd m.Length
        | RegexPattern "\|\|" m -> addToken TLogOr m.Length
        | RegexPattern "&" m ->    addToken TBitAnd m.Length
        | RegexPattern "~" m ->    addToken TBitNot m.Length
        | RegexPattern "\|" m ->   addToken TBitOr m.Length
        | RegexPattern "==" m ->   addToken TEq m.Length
        | RegexPattern "!=" m ->   addToken TNeq m.Length
        | RegexPattern "!" m ->    addToken TLogNot m.Length
        | RegexPattern ">=" m ->   addToken TGte m.Length
        | RegexPattern "<=" m ->   addToken TLte m.Length
        | RegexPattern "<" m ->    addToken TLt m.Length
        | RegexPattern ">" m ->    addToken TGt m.Length
        | RegexPattern "'" m ->    addToken TBusCast m.Length
        | RegexPattern ".*(\n|$)" m ->
            // Catch all syntax to match any invalid input
            Error {
                Pos = {Line = curLine; Col = curCol; Length = m.Length}; 
                Msg = "Unrecognized token: " + m.Value; 
            }
        | _ ->
            failwithf "What? Should not be possible for code not to be matched to token in assertion lexer"

let mapBinaryOpToExpr tokenType binaryOperands =
    match tokenType with
    | TAdd ->
        Expr.Add binaryOperands
    | TSub ->
        Expr.Sub binaryOperands
    | TMul ->
        Expr.Mul binaryOperands
    | TDiv ->
        Expr.Div binaryOperands
    | TRem ->
        Expr.Rem binaryOperands
    | TBitAnd ->
        Expr.BitAnd binaryOperands
    | TBitNot ->
        Expr.BitNot binaryOperands
    | TBitOr ->
        Expr.BitOr binaryOperands
    | TEq ->
        Expr.BoolExpr (BoolExpr.Eq binaryOperands) 
    | TNeq ->
        Expr.BoolExpr (BoolExpr.Neq binaryOperands) 
    | TLogAnd ->
        Expr.BoolExpr (BoolExpr.LogAnd binaryOperands) 
    | TLogNot ->
        Expr.BoolExpr (BoolExpr.LogNot binaryOperands) 
    | TLogOr ->
        Expr.BoolExpr (BoolExpr.LogOr binaryOperands) 
    | TLt ->
        Expr.BoolExpr (BoolExpr.Lt binaryOperands) 
    | TGt ->
        Expr.BoolExpr (BoolExpr.Gt binaryOperands) 
    | TGte ->
        Expr.BoolExpr (BoolExpr.Gte binaryOperands) 
    | TLte ->
        Expr.BoolExpr (BoolExpr.Lte binaryOperands) 
    | _ -> 
        failwithf "What? It is not possible for this token to contain binary operands."


/// See https://class.ece.uw.edu/cadta/verilog/operators.html
let operatorPrecedence (tokType:TokenType):Precedence =
    match tokType with
    | TBusCast ->               Precedence <| Some 110
    | TLogNot ->                Precedence <| Some 100
    | TMul | TDiv | TRem ->     Precedence <| Some 80
    | TAdd | TSub ->            Precedence <| Some 70
    | TGt | TGte | TLt | TLte-> Precedence <| Some 60
    | TEq | TNeq ->             Precedence <| Some 50
    | TBitAnd ->                Precedence <| Some 40
    | TBitOr ->                 Precedence <| Some 30
    | TLogAnd ->                Precedence <| Some 20
    | TLogOr ->                 Precedence <| Some 10
    | _ ->                      Precedence None

/// Small "helper" functions which give a better
/// semantical understand of the parser code.
let nextToken (tokens: Token list): Token = 
    List.head tokens

let popToken (tokens: Token list): Token list = 
    List.tail tokens

let rec parseOperand expectParen (prevToken: Token) (tokens:Token list) : ParseResult =
    if List.isEmpty tokens then
        Error { Msg = "Missing operand!"; Pos = prevToken.Pos}
    else 
        let token = nextToken tokens
        let tokens' = popToken tokens

        let handleParens lParenToken remainingTokens =
            let parenthisedExpr = parseExpr 0 true prevToken remainingTokens
            let handleEndOfParen parenExpr =
                if List.isEmpty parenExpr.RemainingTokens then
                    Error { Msg = "This left parenthesis is not matched."; Pos = lParenToken.Pos }
                else 
                    Ok {parenExpr with RemainingTokens = popToken parenExpr.RemainingTokens}
                    //parseBinaryOp 0 expectParen {parenExpr with RemainingTokens = popToken parenExpr.RemainingTokens}
            Result.bind handleEndOfParen parenthisedExpr

        let handleCast castType =
            if List.isEmpty tokens' then
                Error { Msg = "Expected a subsequent left-parenthesis"; Pos = token.Pos }
            else 
                let lParenToken = nextToken tokens'
                match lParenToken.Type with
                | TLParen -> 
                    handleParens lParenToken <| popToken tokens' 
                    |> Result.bind (fun parenExpr ->
                        let castExpr = Cast (castType (parenExpr.Expr, token.Pos))
                        Ok {parenExpr with Expr = castExpr}
                    )
                | _ -> 
                    Error { Msg = sprintf "Expected a left-parenthesis, got %A" lParenToken.Type; Pos = lParenToken.Pos }

        let handleUnaryOp unaryType =
            parseOperand expectParen prevToken tokens'
            |> Result.bind (fun exprData ->
                let addExpr = unaryType (UnOp (exprData.Expr, token.Pos))
                Ok {Expr = addExpr; RemainingTokens = exprData.RemainingTokens}
            )

        match token.Type with
        | TLit l -> 
            let expr = Expr.Lit l
            Ok {Expr = expr ; RemainingTokens = tokens'}
        | TLParen ->
            handleParens token tokens'
        | TSigned -> handleCast ToSigned
        | TUnsigned -> handleCast ToUnsigned
        | TBool -> handleCast ToBool
        | TAdd -> handleUnaryOp Add
        | TSub -> handleUnaryOp Sub
        | _ ->
            Error {Msg = sprintf "%A is not a valid operand!" token.Type; Pos = token.Pos }

and parseBinaryOp minPrecedence expectParen (lhs:ParseData) :ParseResult =

    if List.isEmpty lhs.RemainingTokens then
        // No more tokens 
        Ok lhs
    else 
        let token = nextToken lhs.RemainingTokens

        let (Precedence prec) = operatorPrecedence token.Type
        match prec with
        | None -> 
            // Peek token is not a binary operator. The only allowed token here is a right-paren
            // given that we are actually expecting one.
            match expectParen, token.Type with
            | true, TRParen ->
                Ok {lhs with RemainingTokens = lhs.RemainingTokens}
            | true, _ ->
                Error { 
                    Msg = sprintf "Expected a right parenthesis or binary operation, got %A" token.Type;
                    Pos = token.Pos;        
                }
            | _ -> 
                Error { 
                    Msg = sprintf "Expected a binary operation, got %A" token.Type;
                    Pos = token.Pos;        
                }

        | Some precedence when precedence < minPrecedence -> Ok lhs  // Peek token does not have a high enough precedence or 
        | Some precedence ->
            // Token is valid binary operator and has high enough precedence to continue
            printfn "Current op: %A" token
            printfn "Remaining tokens: %A" <| List.tail lhs.RemainingTokens

            // Parse the RHS operand of the binary operation
            let rhsRes = parseExpr (precedence + 1) expectParen token <| popToken lhs.RemainingTokens 
            
            let createBinaryExpr rhs =
                let binaryOperands = BinOp ((lhs.Expr, token.Pos), (rhs.Expr, token.Pos))
                let binaryExpr = mapBinaryOpToExpr token.Type binaryOperands

                // The resulting Expr of the binary operation becomes the lhs for the next call!
                parseBinaryOp minPrecedence expectParen <| {Expr = binaryExpr; RemainingTokens = rhs.RemainingTokens}

            Result.bind createBinaryExpr rhsRes

and parseExpr (minPrecedence:int) expectParen (prevToken:Token) (tokens: Token list): ParseResult =
    parseOperand expectParen prevToken tokens
    |> Result.bind (parseBinaryOp minPrecedence expectParen)


let printASTprefix (prefix:string) isLast: string*string =
    if isLast then
        prefix + "└──", prefix + "    "
    else 
        prefix + "├──", prefix + "│  "


/// Helper to only print the type name itself e.g. cases for DU types
let printTypeName t =
    Option.get << String.regexMatch "([^\s]+)" <| sprintf "%A" t

let rec prettyPrintAST expr prefix isLast =
    let curPrefix, newPrefix = printASTprefix prefix isLast

    let printOperand op = 
        match op with
        | UnOp info -> 
            "", prettyPrintAST (fst info) newPrefix true
        | BinOp (left, right) -> 
            "", prettyPrintAST (fst left) newPrefix false + prettyPrintAST (fst right) newPrefix true 

    let addInfo, nextLines = 
        match expr with
        | Add op | Sub op | Mul op | Div op | Rem op 
        | BitOr op | BitNot op | BitAnd op -> 
            printOperand op
        | BoolExpr bExpr ->
            match bExpr with
            | Eq op | Neq op | LogAnd op | LogNot op
            | LogOr op | Lt op | Gt op | Gte op | Lte op ->
                let addInfo, nextLines = printOperand op
                addInfo + printTypeName bExpr, nextLines
        | Lit l -> sprintf "%A" l, ""
        | Cast c ->
            let (ToSigned info| ToUnsigned info | ToBool info) = c
            printTypeName c, prettyPrintAST (fst info) newPrefix true
        | BusCast (size,_)-> sprintf "%A" size, "" 

    sprintf "%A %A %A\n %A" curPrefix (printTypeName expr) addInfo nextLines



/// Returns either the resulting AST or an Error 
let parseAssertion code: Result<Expr, ErrorInfo>=
    printfn "Parsing string32: %A" code

    let pipePrint x =
        printfn "%A" x
        x

    lexAssertion code 1 1 []
    |> Result.bind (fun tokens ->
        if List.isEmpty tokens then
            Error { Msg = "Missing assertion expression!"; Pos = {Line = 1; Col = 1; Length = 1} }
        else 
            pipePrint tokens |> ignore
            parseExpr 0 false (List.head tokens) tokens
            |> pipePrint
    )
    |> Result.map (fun parseData -> 
        printfn "%A" <| prettyPrintAST parseData.Expr "" true
        parseData.Expr

    )
    |> Result.mapError (fun e -> {Message = e.Msg; Line = e.Pos.Line; Col = e.Pos.Col; Length = e.Pos.Length; ExtraErrors = None})
    
    // TODO(jsand): In the above we have to map from our own error type to the error type used by the Verilog code editor.
    // These types should be unified.
