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

    let private matchToken tokenTypes = 
        let matchType token =
            Array.contains token.Type tokenTypes
        matchType

    // General TODO: Thenk of changing tokensList before passing it to other functions

    let private parseComparison tokensList = Literal (null) // Stub to be implemented later
    
    let private parseEquality tokensList = // TODO: Think of fail path
        let left = parseComparison tokensList
        let skipFunc = (fun token -> matchToken [|TokenType.BangEqual; TokenType.EqualEqual|] token)
        let operator = List.takeWhile skipFunc tokensList |> List.rev |> List.head
        let right = parseComparison tokensList

        Binary (left, operator, right)
    
    let private parseExpression tokensList = parseEquality tokensList // To be implemented later

    let private primary list = // Stub to be implemented later
        Literal null
    
    let rec private unary list = 
        let head = List.tryHead list
        match head with
        | None -> primary list
        | _ -> 
            if matchToken [|TokenType.Bang; TokenType.Minus|] head.Value then
                let right = unary (Common.tryTail list)
                Unary (head.Value, right)
            else
                primary list