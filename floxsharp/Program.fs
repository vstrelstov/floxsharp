open System
open System.IO
open Floxsharp.Common

type TokenType = 
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | Comma
    | Dot
    | Minus
    | Plus
    | Semicolon
    | Slash
    | Star
    | Bang
    | BangEqual
    | Equal
    | EqualEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    | Comment
    | Identifier
    | String
    | Number
    | And
    | Class
    | Else
    | False
    | Fun
    | For
    | If
    | Nil
    | Or
    | Print
    | Return 
    | Super
    | This
    | True
    | Var
    | While
    | EOF

type Token =
    {
        Type: TokenType;
        Lexeme: string;
        Line: int;
    }

let rec scanTokens (source: string) =
    let mutable lineNumber = 1

    let createLexemedToken tokenType lexeme = 
        { Type = tokenType; Lexeme = lexeme; Line = lineNumber }

    let createToken tokenType = createLexemedToken tokenType String.Empty
    
    let ignoredSymbols = [|' '; '\r'; '\t'|]
    let skipNextSymbol = [|TokenType.GreaterEqual; TokenType.LessEqual; TokenType.EqualEqual; TokenType.BangEqual|] // TODO: Consider renaming
    let keywords = Map [("and", TokenType.And); ("class", TokenType.Class); ("else", TokenType.Else); ("false", TokenType.False);
        ("for", TokenType.For); ("fun", TokenType.Fun); ("if", TokenType.If); ("nil", TokenType.Nil); ("or", TokenType.Or);
        ("print", TokenType.Print); ("return", TokenType.Return); ("super", TokenType.Super); ("this", TokenType.This);
        ("true", TokenType.True); ("var", TokenType.Var); ("while", TokenType.While)]

    let rec loop source tokens =

        let matchNext expected = Common.peekNextSymbol source = (Some expected)

        let createTokenByNextExpected expectedSymbol matchTokenType mismatchTokenType =
            if matchNext expectedSymbol then
                createToken matchTokenType
            else createToken mismatchTokenType

        let getLongToken src tokenType skipFunction =
            let lexeme = 
                src
                |> List.takeWhile skipFunction
                |> List.map (fun s -> s.ToString())
                |> String.concat ""
                |> (fun s -> s.TrimStart '"')

            createLexemedToken tokenType lexeme

        let createStringToken = 
            getLongToken (Common.tryTail source) TokenType.String (fun c -> 
                if c = '\n' then 
                    lineNumber <- (lineNumber + 1)
                c <> '"')

        let createNumberToken = // TODO: Looks messed up and requires refactoring
            let skipFunc = fun c -> Char.IsDigit c
            let intergerPartToken = getLongToken source TokenType.Number skipFunc
            let afterSkip = List.skip (String.length intergerPartToken.Lexeme) source
            if List.tryHead afterSkip <> Some('.') then
                intergerPartToken
            else
                let fractionalPartToken = getLongToken (Common.tryTail afterSkip) TokenType.Number skipFunc
                createLexemedToken TokenType.Number $"{intergerPartToken.Lexeme}.{fractionalPartToken.Lexeme}"

        let createIdentifierToken = 
            let skipFunc = fun c -> Char.IsLetterOrDigit c
            let token = getLongToken source TokenType.Identifier skipFunc
            if Map.containsKey (token.Lexeme.ToLower()) keywords then
                createLexemedToken keywords.[token.Lexeme.ToLower()] token.Lexeme
            else 
                createLexemedToken TokenType.Identifier token.Lexeme

        let scanToken currentChar =
            match currentChar with
            | '(' -> createToken TokenType.LeftParen
            | ')' -> createToken TokenType.RightParen
            | '{' -> createToken TokenType.LeftBrace
            | '}' -> createToken TokenType.RightBrace
            | ',' -> createToken TokenType.Comma
            | '.' -> createToken TokenType.Dot
            | '-' -> createToken TokenType.Minus
            | '+' -> createToken TokenType.Plus
            | ';' -> createToken TokenType.Semicolon
            | '*' -> createToken TokenType.Star
            | '/' -> createTokenByNextExpected '/' TokenType.Comment TokenType.Slash
            | '!' -> createTokenByNextExpected '=' TokenType.BangEqual TokenType.Bang
            | '=' -> createTokenByNextExpected '=' TokenType.EqualEqual TokenType.Equal
            | '<' -> createTokenByNextExpected '=' TokenType.LessEqual TokenType.Less
            | '>' -> createTokenByNextExpected '=' TokenType.GreaterEqual TokenType.Greater
            | '"' -> createStringToken
            | _ ->
                if Char.IsDigit currentChar then
                    createNumberToken
                elif Char.IsLetter currentChar then 
                    createIdentifierToken
                else raise (Common.InterpreterException (lineNumber, String.Empty, "Unexpected character"))

        match source with // TODO: Looks messed up and requires refactoring
        | [] -> tokens
        | head::tail when Array.contains head ignoredSymbols -> loop tail tokens
        | head::tail when head = '\n' ->
            lineNumber <- (lineNumber + 1)
            loop tail tokens
        | head::tail -> 
            let newToken = scanToken head
            match newToken.Type with
            | x when Array.contains x skipNextSymbol -> loop (Common.tryTail tail) (tokens @ [newToken])
            | TokenType.String -> 
                let afterSkip = List.skip (String.length newToken.Lexeme) tail
                let newHead = List.tryHead afterSkip
                if newHead.IsNone || newHead.Value <> '"' then
                    raise (Common.InterpreterException (lineNumber, String.Empty, "Unterminated string"))
                loop (Common.tryTail afterSkip) (tokens @ [newToken])
            | TokenType.Number
            | TokenType.Identifier -> loop (List.skip (String.length newToken.Lexeme) source) (tokens @ [newToken])
            | TokenType.Comment -> loop (List.skipWhile (fun c -> c <> '\n') tail) (tokens @ [newToken])
            | _ when Map.containsKey newToken.Lexeme keywords -> 
                loop (List.skip (String.length newToken.Lexeme) source) (tokens @ [newToken])
            | _ -> loop tail (tokens @ [newToken])

    loop (source |> Seq.toList) []

let run (source: string) =
    scanTokens source
    |> List.map (fun s -> Console.WriteLine(s.ToString()))
    |> ignore

let runFile (filePath: string) =
    try
        use reader = new StreamReader(filePath)
        reader.ReadToEnd() |> run
    with
    | Common.InterpreterException (line, where, message) -> Common.report line where message
    
    ()

let rec runPrompt () =
    printf "%s" ">> "
    let line = Console.ReadLine()
    if String.IsNullOrWhiteSpace(line) then ()
    else
        try
            run line |> ignore
        with
        | Common.InterpreterException (line, where, message) -> Common.report line where message
        runPrompt ()

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> 
        printfn "%s" "You are entering REPL mode. Press Ctrl+Enter to exit."
        runPrompt ()
    | 1 -> runFile argv.[0]
    | _ ->
        printfn "%s" "Too many parameters. Usage: floxsharp [script_path] | floxsharp (to run in REPL mode)"

    0
