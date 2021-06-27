namespace Floxsharp.Interpreter

open System
open System.Globalization
open Floxsharp.Common
open Floxsharp.Interpreter.Scanning
open Floxsharp.Interpreter.Parsing

module Parser =

    [<Obsolete("This function was used for testing purposes")>]
    let prettyPrint expression = 
        let rec printExpression exp =
            let groupingName = "group"
            match exp with
            | Literal value -> Convert.ToString(value, CultureInfo.InvariantCulture)
            | Unary (operator, right) -> $"{parenthesize operator.Lexeme [right]})"
            | Grouping expr -> $"{parenthesize groupingName [expr]})"
            | Binary (left, operator, right) -> $"{parenthesize operator.Lexeme [left; right]})"
        
        and parenthesize name expressions = 
            let rec loop expressions printedExpressions =
                match expressions with
                | [] -> printedExpressions
                | head::tail -> loop tail (printedExpressions @ [$" {printExpression head}"])
            
            loop expressions [$"({name}"] |> String.concat ""

        printExpression expression

    
