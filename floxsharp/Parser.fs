namespace Floxsharp.Interpreter

open System
open System.Globalization
open Floxsharp.Interpreter.Parsing

module Parser =
    [<Obsolete("This function was used for testing purposes")>]
    let printExpression expression = 
        let rec matchExpressionType exp =
            let groupingName = "group"
            match exp with
            | Literal value -> Convert.ToString(value, CultureInfo.InvariantCulture)
            | Unary (operator, right) -> $"{parenthesize operator.Lexeme [right]})"
            | Grouping expr -> $"{parenthesize groupingName [expr]})"
            | Binary (left, operator, right) -> $"{parenthesize operator.Lexeme [left; right]})"
        
        and parenthesize name expressions = 
            let rec loop expressions lst = // TODO: Rename lst
                match expressions with
                | [] -> lst
                | head::tail -> loop tail (lst @ [$" {matchExpressionType head}"])
            
            loop expressions [$"({name}"] |> String.concat ""

        matchExpressionType expression
        