open System
open System.IO

type RunState = 
    {
        HasError: bool;
    }

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
    } // TODO: Implement Tostring() function

let scanTokens (source: string) =
    List.empty<string>

let run (source: string) =
    scanTokens source
    |> List.map (fun s -> Console.WriteLine(s))
    |> ignore
    { HasError = false; }

let runFile (filePath: string) =
    use reader = new StreamReader(filePath)
    let result = run (reader.ReadToEnd()) 
    match result.HasError with
    | true -> failwith "An error occured. Execution aborted."
    | false -> ()

let rec runPrompt () =
    printf "%s" ">> "
    let line = Console.ReadLine()
    match String.IsNullOrWhiteSpace(line) with
    | true -> ()
    | false -> 
        run line |> ignore
        runPrompt ()

let error line message = 
    let report line where message =
        Console.WriteLine($"[line {line}] Error: {where}: {message}")
        { HasError = true; }
    report line String.Empty message

[<EntryPoint>]
let main argv =
    match argv.Length with
    | 0 -> runPrompt ()
    | 1 -> runFile argv.[0]
    | _ -> failwith "Too many parameters. Usage: floxsharp [script_path]"

    0
