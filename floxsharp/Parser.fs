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
        let matchType list = 
            let head = List.tryHead list
            Option.isSome head && Array.contains head.Value.Type tokenTypes
        matchType

    let private parseComparison () = () // Stub to be implemented later
    
    let private parseEquality tokensList = 
        let equalityTokenTypes = [|TokenType.BangEqual; TokenType.EqualEqual|]
        
        Binary (Literal (null), { Type = TokenType.EOF; Lexeme = ""; Line = 0}, Literal (null))// TODO: Replace stub values
    
    let private parseExpression () = parseEquality List.empty<Token> // Stub to be implemented later

    let private primary list = // Stub function
        Literal null
    
    let rec private unary list = 
        if matchToken [|TokenType.Bang; TokenType.Minus|] list then
            let operator = List.tryHead list // TODO: Probably should raise an exception if list is empty
            let right = unary (Common.tryTail list)
            Unary (operator.Value, right)
        else 
            primary list // TODO: Modify parameter if needed
    