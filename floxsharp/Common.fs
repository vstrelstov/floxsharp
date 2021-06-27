namespace Floxsharp.Common

[<AutoOpen>]
module Common =
    open System 

    let tryTail list =
        match list with
        | [] -> list
        | _::tail -> tail
    
    let peekNextSymbol list = List.tryHead (tryTail list)

    exception InterpreterException of int * string * string

    let report line where message = Console.WriteLine($"[line {line}] Error: {message}")