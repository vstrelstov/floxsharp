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

let peekNextSymbol list = List.tryHead (tryTail list)

let rec scanTokens (source: string) =
    let mutable lineNumber = 1

    let createToken tokenType lexeme = 
        { Type = tokenType; Lexeme = lexeme; Line = lineNumber }
    let createToken tokenType = createToken tokenType String.Empty
    
    let ignoredSymbols = [|' '; '\r'; '\t'|]
    let skipNextSymbol = [|TokenType.GreaterEqual; TokenType.LessEqual; TokenType.EqualEqual; TokenType.BangEqual; TokenType.Comment|] // TODO: Consider renaming

    let rec loop source tokens =

        let matchNext expected = peekNextSymbol source = (Some expected)

        let createTokenByNextExpected expectedSymbol matchTokenType mismatchTokenType =
            if matchNext expectedSymbol then
                createToken matchTokenType
            else createToken mismatchTokenType

        let getLongToken tokenType skipFunction =
            let lexeme = 
                tryTail source
                |> List.takeWhile skipFunction
                |> List.map (fun s -> s.ToString())
                |> String.concat ""

            { Type = tokenType; Lexeme = lexeme; Line = lineNumber }

        let rec createStringToken = 
            let skipFunc = fun c -> 
                if c = '\n' then 
                    lineNumber <- (lineNumber + 1)
                c <> '"'

            getLongToken TokenType.String skipFunc

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
            | '/' -> createTokenByNextExpected '/' TokenType.Comment TokenType.Slash
            | '!' -> createTokenByNextExpected '=' TokenType.BangEqual TokenType.Bang
            | '=' -> createTokenByNextExpected '=' TokenType.EqualEqual TokenType.Equal
            | '<' -> createTokenByNextExpected '=' TokenType.LessEqual TokenType.Less
            | '>' -> createTokenByNextExpected '=' TokenType.GreaterEqual TokenType.Greater
            | '"' -> createStringToken
            | _ -> raise (InterpreterException (lineNumber, String.Empty, "Unexpected character"))

        match source with // TODO: Requires refactoring
        | [] -> tokens
        | head::tail when Array.contains head ignoredSymbols -> loop tail tokens
        | head::tail when head = '\n' ->
            lineNumber <- (lineNumber + 1)
            loop tail tokens
        | head::tail -> 
            let newToken = scanToken head
            match newToken.Type with
            | TokenType.GreaterEqual | TokenType.LessEqual | TokenType.EqualEqual 
            | TokenType.BangEqual -> loop (tryTail tail) (tokens @ [newToken])
            | TokenType.String -> // TODO: Может, вынесешь этот пиздец куда-нибудь? 
                let afterSkip = List.skip (String.length newToken.Lexeme) tail
                let h = List.tryHead afterSkip
                if h.IsSome && h.Value <> '"' then
                    raise (InterpreterException (lineNumber, String.Empty, "Unterminated string"))
                loop (tryTail afterSkip) (tokens @ [newToken])
            | TokenType.Comment -> loop (List.skipWhile (fun c -> c <> '\n') tail) (tokens @ [newToken])
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
    if String.IsNullOrWhiteSpace(line) then ()
    else
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
