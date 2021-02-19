open System
open System.IO

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
        // TODO: Add Literal: Object
    }

exception InterpreterException of int * string * string

let report line where message =
    Console.WriteLine($"[line {line}] Error: {message}")

let tryTail list =
    match list with
    | [] -> list
    | head::tail -> tail

let rec scanTokens (source: string) =
    let mutable lineNumber = 1

    let createToken tokenType lexeme = 
        { Type = tokenType; Lexeme = lexeme; Line = lineNumber }
    let createToken tokenType = createToken tokenType String.Empty

    let ignoredSymbols = [|' '; '\r'; '\t'|]
    let skipNextSymbol = [|TokenType.GreaterEqual; TokenType.LessEqual; TokenType.EqualEqual; TokenType.BangEqual|] // TODO: Consider renaming

    let rec loop source tokens =
        let peekNextSymbol = List.tryHead (tryTail source)

        let matchNext expected = peekNextSymbol = (Some expected)

        let createTokenByCondition symbolToMatch matchTokenType mismatchTokenType =
            match matchNext symbolToMatch with
            | true -> createToken matchTokenType
            | false -> createToken mismatchTokenType

        let rec createStringToken = 
            
            {Type = TokenType.String; Lexeme = String.Empty; Line = 0;} // Stub

        let scanToken = function
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
            | '/' -> createTokenByCondition '/' TokenType.Comment TokenType.Slash
            | '!' -> createTokenByCondition '=' TokenType.BangEqual TokenType.Bang
            | '=' -> createTokenByCondition '=' TokenType.EqualEqual TokenType.Equal
            | '<' -> createTokenByCondition '=' TokenType.LessEqual TokenType.Less
            | '>' -> createTokenByCondition '=' TokenType.GreaterEqual TokenType.Greater
            | '"' -> createStringToken
            | _ -> raise (InterpreterException (lineNumber, String.Empty, "Unexpected character"))
        // TODO: set lineNubmer
        match source with // TODO: Requires refactoring
        | [] -> tokens
        | head::tail -> 
            match Array.contains head ignoredSymbols with 
            | true -> loop tail tokens
            | false -> 
                let newToken = scanToken head
                match Array.contains newToken.Type skipNextSymbol with
                | true -> loop (tryTail tail) (tokens @ [newToken])
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
    | InterpreterException (line, where, message) -> report line where message
    
    ()

let rec runPrompt () =
    printf "%s" ">> "
    let line = Console.ReadLine()
    match String.IsNullOrWhiteSpace(line) with
    | true -> ()
    | false -> 
        try
            run line |> ignore
        with
        | InterpreterException (line, where, message) -> report line where message
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
