namespace Floxsharp.Common

open Floxsharp.Interpreter.Scanning

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
        
    let reportTokenError token message =
        match token.Type with
        | TokenType.EOF -> report token.Line "at end" message
        | _ -> report token.Line $"at '{token.Lexeme}'" message
