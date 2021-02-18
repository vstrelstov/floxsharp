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
    } // TODO: Implement Tostring() function if needed

exception InterpreterException of int * string * string

let report line where message =
    Console.WriteLine($"[line {line}] Error: {message}")

let rec scanTokens (source: string) =
    let mutable lineNumber = 1

    let addToken tokenType lexeme = 
        { Type = tokenType; Lexeme = lexeme; Line = lineNumber }
    let addToken tokenType = addToken tokenType String.Empty

    let rec loop source tokens = 
        let scanToken = function
            | '(' -> addToken TokenType.LeftParen
            | ')' -> addToken TokenType.RightParen
            | '{' -> addToken TokenType.LeftBrace
            | '}' -> addToken TokenType.RightBrace
            | ',' -> addToken TokenType.Comma
            | '.' -> addToken TokenType.Dot
            | '-' -> addToken TokenType.Minus
            | '+' -> addToken TokenType.Plus
            | ';' -> addToken TokenType.Semicolon
            | '*' -> addToken TokenType.Star 
            | _ -> raise (InterpreterException (lineNumber, String.Empty, "Unexpected character")) 
            
        match source with
        | [] -> tokens
        | head::tail -> 
            match head with
            | '\n' -> 
                lineNumber <- (lineNumber + 1)
                loop tail tokens
            | _ -> loop tail (tokens @ [scanToken head])

    loop (source |> Seq.toList |> List.filter (fun c -> c <> ' ' && c <> '\r' && c <> '\t')) []

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
        printfn "%s" "You are entering REPL mode. Press Ctrl+C to exit."
        runPrompt ()
    | 1 -> runFile argv.[0]
    | _ ->
        printfn "%s" "Too many parameters. Usage: floxsharp [script_path] | floxsharp (to run in REPL mode)"

    0
