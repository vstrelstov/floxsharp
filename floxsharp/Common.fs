﻿namespace Floxsharp.Common

module Common =
    open System 

    let tryTail list =
        match list with
        | [] -> list
        | head::tail -> tail
    
    let peekNextSymbol list = List.tryHead (tryTail list)

    exception InterpreterException of int * string * string

    let report line where message = Console.WriteLine($"[line {line}] Error: {message}")