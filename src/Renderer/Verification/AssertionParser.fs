(*
    AssertionParser.fs

    Hand-written parser for assertion logic.
    Author: jlsand
*)

module AssertionParser

open EEExtensions
open AssertionTypes

let (|RegexPattern|_|) regex str =
    let regex' = "^" + regex 
    String.regexMatchFull regex' str

/// Lex an assertion expression into a series of tokens.
/// Will supply errors messages pointing to the piece of text
/// which cannot be correctly tokenized.
let rec lexAssertion (code: string) curLine curCol (tokens: Token list): Result<Token list, CodeError> =
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
        | RegexPattern "\/\/[^\n\r]+" m -> 
            // Single line comment
            lexAssertion (code.Substring m.Length) curLine (curCol + m.Length) tokens
        | RegexPattern "[1-9]\d*'" m ->
            let matchedStr = m.Value;
            let width = System.Int32.Parse <| matchedStr.Remove (matchedStr.Length - 1)
            addToken (TBusCast width) m.Length
        | RegexPattern "'" m ->
            Error { 
                Pos = {Line = curLine; Col = curCol; Length = m.Length}
                Msg = "Remember to specify a width for the bus cast"
                ExtraErrors = None
            }
        | RegexPattern "([1-9]\d*|0x([0-9]|[A-F])+|0b(0|1)+)" m -> 
            // integer literals
            let intTok = System.Int32.Parse m.Value |> Int |> Value |> TLit 
            addToken intTok m.Length
        | RegexPattern "signed" m -> addToken TSigned m.Length 
        | RegexPattern "unsigned" m -> addToken TUnsigned m.Length 
        | RegexPattern "bool" m -> addToken TBool m.Length 
        | RegexPattern "assertTrue" m -> addToken TAssertTrue m.Length 
        | RegexPattern "assertFalse" m -> addToken TAssertFalse m.Length 
        | RegexPattern "input" m -> addToken TInput m.Length
        | RegexPattern "[_a-zA-Z][_a-zA-Z0-9]*" m ->
            // identifiers
            // ln220: added 0 as a temporary fix to the fact that now output port number is fixed
            let idTok = Id (m.Value, 0) |> TLit
            addToken idTok m.Length
        | RegexPattern "," m ->    addToken TComma m.Length 
        | RegexPattern ";" m ->    addToken TComma m.Length 
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
        | RegexPattern ".*(\n|$)" m ->
            // Catch all syntax to match any invalid input
            Error {
                Pos = {Line = curLine; Col = curCol; Length = m.Length}; 
                Msg = "Unrecognized token: " + m.Value; 
                ExtraErrors = None
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

let createExpectError (expected:string) (tokenIsPrevious:bool) (token: Token)  =
    match tokenIsPrevious with
    | true ->
        { Msg = sprintf "Expected a %A after %A" expected <| tokenSymbol token.Type; Pos = token.Pos; ExtraErrors = None }
    | false ->
        { Msg = sprintf "Expected a %A, got %A" expected <| tokenSymbol token.Type; Pos = token.Pos; ExtraErrors = None }

/// Parse an operand, potentially being prefixed by unary operations (e.g. casts)
/// or contained in a set of parentheses.
let rec parseOperand expectParen (prevToken: Token) (tokens:Token list) : ParseResult =
    if List.isEmpty tokens then
        Error { Msg = "Missing operand!"; Pos = prevToken.Pos; ExtraErrors = None}
    else 
        let token = nextToken tokens
        let tokens' = popToken tokens

        let handleParens lParenToken remainingTokens =
            let parenthisedExpr = parseExpr 0 true prevToken remainingTokens
            let handleEndOfParen parenExpr =
                if List.isEmpty parenExpr.RemainingTokens then
                    Error { Msg = "This left parenthesis is not matched."; Pos = lParenToken.Pos; ExtraErrors = None }
                else 
                    Ok {parenExpr with RemainingTokens = popToken parenExpr.RemainingTokens}
            Result.bind handleEndOfParen parenthisedExpr

        let handleFunctionCall functionType =
            if List.isEmpty tokens' then
                Error <| createExpectError "subsequent left-parenthesis" true token
            else 
                let lParenToken = nextToken tokens'
                match lParenToken.Type with
                | TLParen -> 
                    handleParens lParenToken <| popToken tokens' 
                    |> Result.bind (fun parenExpr ->
                        let castExpr = Cast (functionType (parenExpr.Expr, token.Pos))
                        Ok {parenExpr with Expr = castExpr}
                    )
                | _ -> 
                    Error <| createExpectError "left-parenthesis" false lParenToken


        let handleUnaryOp unaryType =
            parseOperand expectParen prevToken tokens'
            |> Result.bind (fun exprData ->
                let addExpr = unaryType (UnOp (exprData.Expr, token.Pos))
                Ok {Expr = addExpr; RemainingTokens = exprData.RemainingTokens}
            )

        match token.Type with
        | TLit l -> 
            let litExpr = Expr.Lit l
            Ok {Expr = litExpr ; RemainingTokens = tokens'}
        | TLParen -> handleParens token tokens'
        | TSigned -> handleFunctionCall ToSigned
        | TUnsigned -> handleFunctionCall ToUnsigned
        | TBool -> handleFunctionCall ToBool
        //| TAssertTrue -> handleFunctionCall  
        | TAdd -> handleUnaryOp Add
        | TSub -> handleUnaryOp Sub
        | TBusCast width ->
            parseOperand expectParen prevToken tokens'
            |> Result.bind (fun castOperand ->
                let castExpr = BusCast (width, (castOperand.Expr, token.Pos))
                Ok {Expr = castExpr; RemainingTokens = castOperand.RemainingTokens}
            )
        | _ ->
            Error {Msg = sprintf "%A is not a valid operand!" <| tokenSymbol token.Type; Pos = token.Pos; ExtraErrors = None }

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
            | true, TRParen -> Ok {lhs with RemainingTokens = lhs.RemainingTokens}
            | true, _ -> Error <| createExpectError "right-parenthesis or binary operation" false token 
            | _ -> Error <| createExpectError "binary operation" false token

        | Some precedence when precedence < minPrecedence -> Ok lhs  // Peek token does not have a high enough precedence or 
        | Some precedence ->
            // Token is valid binary operator and has high enough precedence to continue

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


let advanceIfNotEmpty (expected:string) (prevToken: Token, tokens: Token list)  =
    if List.isEmpty tokens then
        Error <| createExpectError expected true prevToken
    else
        Ok (prevToken, tokens)

let advanceIfCorrectToken (expected:string) (expectedToken: TokenType) (_, tokens: Token list) =
    let curToken = nextToken tokens
    let tokens' = popToken tokens
    if curToken.Type = expectedToken then
        Ok (curToken, tokens')
    else 
        Error <|createExpectError expected false curToken



let rec parseInputs (inputs:string list) (prevToken:Token) (tokens:Token list) : Result<string list, CodeError> =
    advanceIfNotEmpty "input" (prevToken, tokens)
    |> Result.bind (advanceIfCorrectToken "input" TInput)
    |> Result.bind (advanceIfNotEmpty "identifier")
    |> Result.bind ( fun (_, tokens) ->
        // We have successfully parsed an input token and confirmed that
        // there exists a subsequent token. Now make sure it is an identifier:

        let curToken = nextToken tokens
        let tokens' = popToken tokens
        match curToken.Type with
        | TLit l ->
            match l with
            | Id (name, _) -> 
                advanceIfNotEmpty "semicolon" (curToken, tokens')
                |> Result.bind (advanceIfCorrectToken "semicolon" TSemicolon)
                |> Result.bind ( fun (colonToken, tokens'') ->
                    let inputs' = List.append inputs [name]

                    if List.isEmpty tokens'' then
                        Ok inputs'
                    else
                        let postInputToken = nextToken tokens
                        match postInputToken.Type with 
                        | TInput -> parseInputs inputs' colonToken tokens'' 
                        | _ -> Ok inputs'
                )
            | _ -> Error <| createExpectError "identifier" false curToken
        | _ -> Error <| createExpectError "identifier" false curToken
        )
        



/// Generate a pretty print string for the generated AST. 
let rec prettyPrintAST expr prevPrefix isLast:string =

    // Helper to only print the type name itself e.g. cases for DU types
    // rather than the full type
    let printTypeName t =
        sprintf "%A" t
        |> String.regexMatch "([^\s]+)"
        |> Option.defaultValue ""
    
    // Fable will append a newline to every printf, which is why
    // a lot of strings here need to be stored and then concatenated
    // at the end together.
    let curPrefix, newPrefix =
        if isLast then
            prevPrefix + "└──", prevPrefix + "    "
        else 
            prevPrefix + "├──", prevPrefix + "│  "

    let printOperand op = 
        match op with
        | UnOp info -> 
            "", prettyPrintAST (fst info) newPrefix true
        | BinOp (left, right) -> 
            let leftAST = prettyPrintAST (fst left) newPrefix false
            let rightAST = prettyPrintAST (fst right) newPrefix true 
            "", leftAST + rightAST 

    let operandInfo, childLines = 
        match expr with
        | Add op | Sub op | Mul op | Div op | Rem op 
        | BitOr op | BitNot op | BitAnd op -> 
            printOperand op
        | BoolExpr bExpr ->
            match bExpr with
            | Eq op | Neq op | LogAnd op | LogNot op
            | LogOr op | Lt op | Gt op | Gte op | Lte op ->
                let boolOpInfo, boolOpAST = printOperand op
                boolOpInfo + printTypeName bExpr, boolOpAST
        | Lit l -> sprintf "%A" l, ""
        | Cast c ->
            let (ToSigned info| ToUnsigned info | ToBool info) = c
            let castInfo = printTypeName c
            let castAST = prettyPrintAST (fst info) newPrefix true
            castInfo, castAST 
        | BusCast (size, info)-> 
            sprintf "%A" size, prettyPrintAST (fst info) newPrefix true 

    // Print the current AST node and its children
    sprintf "%A %A %A\n %A" curPrefix (printTypeName expr) operandInfo childLines


/// Returns either the resulting AST or an Error 
let parseAssertion code: Result<Expr, CodeError>=

    lexAssertion code 1 1 []
    |> Result.bind (fun tokens -> 
        if List.isEmpty tokens then
            Error { Msg = "Missing definition of inputs!"; Pos = {Line = 1; Col = 1; Length = 1}; ExtraErrors = None }
        else 
            parseInputs [] (List.head tokens) tokens 
    )
    |> Result.bind (fun parseData ->
        let tokens = parseData.RemainingTokens
        if List.isEmpty tokens then
            Error { Msg = "Missing assertion expression!"; Pos = {Line = 1; Col = 1; Length = 1}; ExtraErrors = None }
        else 
            parseExpr 0 false (List.head tokens) tokens
    )
    |> Result.map (fun parseData -> 
        printfn "%A" <| prettyPrintAST parseData.Expr "" true
        parseData.Expr
    )
    
    // TODO(jsand): In the above we have to map from our own error type to the error type used by the Verilog code editor.
    // These types should be unified in the group stage.
