namespace Floxsharp.Interpreter.Parsing

open Floxsharp.Interpreter.Scanning

[<AutoOpen>]
module ParserTypes =
    type Expression = 
        | Binary of Left: Expression * Operator: Token * Right: Expression
        | Unary of Operator: Token * Right: Expression
        | Grouping of Expression: Expression
        | Literal of Value: obj // Token types: Number, String, True, False, Nil
