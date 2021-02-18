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
    Console.WriteLine($"[line {line}] Error: {where}: {message}")

let scanTokens (source: string) =
    let addToken tokenType lexeme = 
        { Type = tokenType; Lexeme = lexeme; Line = 0 } // TODO: Find how to set line number
    let addToken tokenType = addToken tokenType String.Empty
    
    let scanToken index currentChar =
        match currentChar with
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
        | _ -> raise (InterpreterException (0, String.Empty, "Unexpected character")) 
        // TODO: Find how to set line number
        
    source
    |> Seq.toList
    |> List.mapi (fun index currentChar -> scanToken index currentChar)

let run (source: string) =
    scanTokens source
    |> List.map (fun s -> Console.WriteLine(s))
    |> ignore

let runFile (filePath: string) =
    try
        use reader = new StreamReader(filePath)
        reader.ReadToEnd()
        |> (fun s -> s.Split [|'\n'|])
        |> Array.iter (fun source -> run source) // TODO: Consider using Array.iteri in order to pass line number
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
    | 0 -> runPrompt ()
    | 1 -> runFile argv.[0]
    | _ ->
        printfn "%s" "Too many parameters. Usage: floxsharp [script_path] | floxsharp (to run in REPL mode)"

    0
