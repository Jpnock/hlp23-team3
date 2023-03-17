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
            let token = {Type = tokType; Pos = {Line = curLine; Col = curCol; Length = len; CompId = "parser"}}
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
                Pos = {Line = curLine; Col = curCol; Length = m.Length; CompId = "textBased"}
                Msg = "Remember to specify a width for the bus cast"
                ExtraErrors = None
            }
        | RegexPattern "([1-9]\d*|0x([0-9]|[A-F])+|0b(0|1)+)" m -> 
            // integer literals
            let intTok = System.Int64.Parse m.Value |> Int |> Value |> TLit 
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
            let idTok = Id (m.Value, 0, "") |> TLit
            addToken idTok m.Length
        | RegexPattern "," m ->    addToken TComma m.Length 
        | RegexPattern ";" m ->    addToken TSemicolon m.Length 
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
                Pos = {Line = curLine; Col = curCol; Length = m.Length; CompId = ""}; 
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


let createExpectError (expected:string) (tokenIsPrevious:bool) (token: Token)  =
    match tokenIsPrevious with
    | true ->
        { Msg = sprintf "Expected a %A after %A" expected <| tokenSymbol token.Type; Pos = token.Pos; ExtraErrors = None }
    | false ->
        { Msg = sprintf "Expected a %A, got %A" expected <| tokenSymbol token.Type; Pos = token.Pos; ExtraErrors = None }

/// Small "helper" functions which give a better
/// semantical understand of the parser code.
let nextToken (tokens: Token list): Token = 
    List.head tokens

let popToken (tokens: Token list): Token list = 
    List.tail tokens

/// Go to the next token unconditionally in the token stream.
/// Since this has no check for whether there are any remaining tokens,
/// this should only be called after advanceIfNotEmpty or a similar check.
let advanceStream (stream:TokenStream) =
    {
        CurToken = nextToken stream.RemainingTokens; 
        RemainingTokens = popToken stream.RemainingTokens
    }

let advanceIfNotEmpty (expected:string) (stream: TokenStream) =
    if List.isEmpty stream.RemainingTokens then
        Error <| createExpectError expected true stream.CurToken
    else
        Ok stream

let advanceIfCorrectToken (expectedMsg:string) (expectedType: TokenType) (stream:TokenStream) =
    let stream' = advanceStream stream
    let token = stream'.CurToken 

    if token.Type = expectedType then
        Ok stream'
    else 
        Error <|createExpectError expectedMsg false token

/// Parse an operand, potentially being prefixed by unary operations (e.g. casts)
/// or contained in a set of parentheses.
let rec parseOperand expectParen (inputs: string Set) (stream: TokenStream) : ParseExprResult =
    if List.isEmpty stream.RemainingTokens then
        Error { Msg = "Missing operand!"; Pos = stream.CurToken.Pos; ExtraErrors = None}
    else
        let stream' = advanceStream stream
        let curToken = stream'.CurToken

        let handleParens lParenToken (newStream:TokenStream) =
            let parenthisedExpr = parseExpr 0 true inputs newStream
            let handleEndOfParen parenExpr =
                if List.isEmpty parenExpr.Stream.RemainingTokens then
                    Error { Msg = "This left parenthesis is not matched."; Pos = lParenToken.Pos; ExtraErrors = None }
                else 
                    Ok {parenExpr with Stream = advanceStream parenExpr.Stream}
            Result.bind handleEndOfParen parenthisedExpr

        let handleFunctionCall functionType =
            if List.isEmpty stream'.RemainingTokens then
                Error <| createExpectError "subsequent left-parenthesis" true curToken
            else 
                let lParenToken = nextToken stream'.RemainingTokens
                match lParenToken.Type with
                | TLParen -> 
                    handleParens lParenToken <| advanceStream stream' 
                    |> Result.bind (fun parenExpr ->
                        let castExpr = Cast (functionType (parenExpr.Expr, curToken.Pos))
                        Ok {parenExpr with Expr = castExpr}
                    )
                | _ -> 
                    Error <| createExpectError "left-parenthesis" false lParenToken


        let handleUnaryOp unaryType =
            parseOperand expectParen inputs stream'
            |> Result.bind (fun exprData ->
                let addExpr = unaryType (UnOp (exprData.Expr, curToken.Pos))
                Ok {Expr = addExpr; Stream = exprData.Stream}
            )

        match stream'.CurToken.Type with
        | TLit lit ->
            let litExpr = Expr.Lit lit
            let res = Ok {Expr = litExpr ; Stream = stream'}
            match lit with
            | Id (name,_,_) -> 
                if Set.contains name inputs then
                    res
                else 
                    Error { Msg = "This identifier is not defined as an input."; Pos = curToken.Pos; ExtraErrors = None }
            | _ ->
                res
        | TLParen -> handleParens curToken stream'
        | TSigned -> handleFunctionCall ToSigned
        | TUnsigned -> handleFunctionCall ToUnsigned
        | TBool -> handleFunctionCall ToBool
        //| TAssertTrue -> handleFunctionCall  
        | TAdd -> handleUnaryOp Add
        | TSub -> handleUnaryOp Sub
        | TBusCast width ->
            parseOperand expectParen inputs stream'
            |> Result.bind (fun castOperand ->
                let castExpr = BusCast (width, (castOperand.Expr, curToken.Pos))
                Ok {Expr = castExpr; Stream = castOperand.Stream}
            )
        | _ ->
            Error {Msg = sprintf "%A is not a valid operand!" <| tokenSymbol curToken.Type; Pos = curToken.Pos; ExtraErrors = None }

and parseBinaryOp minPrecedence expectParen (inputs: string Set) (lhs:ParsedExpr) :ParseExprResult =

    if List.isEmpty lhs.Stream.RemainingTokens then
        // No more tokens 
        Ok lhs
    else 
        let token = nextToken lhs.Stream.RemainingTokens

        let (Precedence prec) = operatorPrecedence token.Type
        match prec with
        | None -> 
            // Peek token is not a binary operator. The only allowed token here is a right-paren
            // given that we are actually expecting one.
            match expectParen, token.Type with
            | true, TRParen -> Ok lhs
            | true, _ -> Error <| createExpectError "right-parenthesis or binary operation" false token 
            | _ -> Error <| createExpectError "binary operation" false token

        | Some precedence when precedence < minPrecedence -> Ok lhs  // Peek token does not have a high enough precedence or 
        | Some precedence ->
            // Token is valid binary operator and has high enough precedence to continue

            // Parse the RHS operand of the binary operation
            let rhsRes = parseExpr (precedence + 1) expectParen inputs <| advanceStream lhs.Stream
            
            let createBinaryExpr rhs =
                let binaryOperands = BinOp ((lhs.Expr, token.Pos), (rhs.Expr, token.Pos))
                let binaryExpr = mapBinaryOpToExpr token.Type binaryOperands

                // The resulting Expr of the binary operation becomes the lhs for the next call!
                parseBinaryOp minPrecedence expectParen inputs <| {Expr = binaryExpr; Stream = rhs.Stream}

            Result.bind createBinaryExpr rhsRes

and parseExpr (minPrecedence:int) expectParen (inputs: string Set) (stream: TokenStream): ParseExprResult =
    parseOperand expectParen inputs stream
    |> Result.bind (parseBinaryOp minPrecedence expectParen inputs)

let startParseExpr (parsedInputs: ParsedInputs): Result<Assertion, CodeError> =
    parseExpr 0 false parsedInputs.InputNames parsedInputs.Stream
    |> Result.bind ( fun pExpr -> 
        Ok {AssertExpr = pExpr.Expr,pExpr.Stream.CurToken.Pos; InputNames = parsedInputs.InputNames}
    )

let rec parseInputs (inputs:string Set) (stream:TokenStream) : Result<ParsedInputs, CodeError> =
    advanceIfNotEmpty "input" stream 
    |> Result.bind (advanceIfCorrectToken "input" TInput)
    |> Result.bind (advanceIfNotEmpty "identifier")
    |> Result.bind ( fun stream ->
        // We have successfully parsed an input token and confirmed that
        // there exists a subsequent token. Now make sure it is an identifier:

        let stream' = advanceStream stream
        let token = stream'.CurToken
        match stream'.CurToken.Type with
        | TLit l ->
            match l with
            | Id (name, _,_) -> 
                advanceIfNotEmpty "semicolon" stream'
                |> Result.bind (advanceIfCorrectToken "semicolon" TSemicolon)
                |> Result.bind ( fun stream ->
                    let inputs' = Set.add name inputs

                    if List.isEmpty stream.RemainingTokens then
                        Ok {InputNames = inputs'; Stream = stream}
                    else
                        let postInputToken = nextToken stream.RemainingTokens
                        match postInputToken.Type with 
                        | TInput -> parseInputs inputs' <| advanceStream stream'
                        | _ -> Ok {InputNames = inputs'; Stream = stream}
                )
            | _ -> Error <| createExpectError "identifier" false token
        | _ -> Error <| createExpectError "identifier" false token
        )
        


//let rec parseInputs (tokens:Token list) : ParseResult =



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

let prettyPrintAssertion assertion =
    printfn "Defined inputs: %A" assertion.InputNames
    printfn "%A" <| prettyPrintAST (fst assertion.AssertExpr) "" true
    assertion


/// Returns either the resulting AST or an Error 
let parseAssertion code: Result<Assertion, CodeError>=

    let checkIfMissing (missingMsg:string) (stream: TokenStream) =
        if List.isEmpty stream.RemainingTokens then
            Error { Msg = missingMsg; Pos = stream.CurToken.Pos; ExtraErrors = None }
        else
            Ok stream

    let dummyToken = {Type = TComma; Pos = {Line = 1; Col = 1; Length = 1; CompId = ""}}

    // Parse the inputs list followed by the actual assertion expression.
    // Before each parse check if there are any tokens left and if not
    // return a relevant error.
    lexAssertion code 1 1 []
    |> Result.bind (fun tokens -> Ok {CurToken = dummyToken; RemainingTokens = tokens})
    |> Result.bind (checkIfMissing "Missing definition of inputs!")
    |> Result.bind (parseInputs Set.empty)
    |> Result.bind (fun parsedInputs ->
        checkIfMissing "Missing assertion expression!" parsedInputs.Stream
        |> Result.bind (fun _ -> Ok parsedInputs)
    )
    |> Result.bind (startParseExpr)
    |> Result.map prettyPrintAssertion
