open System
open System.IO

type RunState = 
    {
        HasError: bool;
    }

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
