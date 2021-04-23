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

    // TODO: Think of fail path
    // TODO: Find another way to get operator
    let private getOperator tokensList = 
        let skip skipFunc =
            List.takeWhile skipFunc tokensList |> List.rev |> List.head
        skip 

    // TODO: Parsing functions require refactoring

    let private parsePrimary list = // Stub to be implemented later
        Literal null

    let rec private parseUnary tokensList = 
        let head = List.tryHead tokensList
        match head with
        | None -> parsePrimary tokensList
        | _ -> 
            if matchToken [|TokenType.Bang; TokenType.Minus|] head.Value then
                let right = parseUnary (Common.tryTail tokensList)
                Unary (head.Value, right)
            else
                parsePrimary tokensList

    let private parseFactor tokensList = 
        let left = parseUnary tokensList
        let skipFunc = matchToken [|TokenType.Slash; TokenType.Star|]
        let operator = getOperator tokensList skipFunc
        let right = parseUnary (List.skipWhile skipFunc tokensList)

        Binary (left, operator, right)

    let private parseTerm tokensList = 
        let left = parseFactor tokensList
        let skipFunc = matchToken [|TokenType.Minus; TokenType.Plus|]
        let operator = getOperator tokensList skipFunc
        let right = parseFactor (List.skipWhile skipFunc tokensList)
        
        Binary (left, operator, right)

    let private parseComparison tokensList = 
        let left = parseTerm tokensList
        let skipFunc = matchToken [|TokenType.Greater; TokenType.GreaterEqual; TokenType.Less; TokenType.LessEqual|]
        let operator = getOperator tokensList skipFunc
        let right = parseTerm (List.skipWhile skipFunc tokensList)
        
        Binary (left, operator, right)
    
    let private parseEquality tokensList =
        let left = parseComparison tokensList
        let skipFunc = matchToken [|TokenType.BangEqual; TokenType.EqualEqual|]
        let operator = getOperator tokensList skipFunc
        let right = parseComparison (List.skipWhile skipFunc tokensList)

        Binary (left, operator, right)
    
    let private parseExpression tokensList = parseEquality tokensList