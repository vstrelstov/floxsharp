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
    // TODO: Find another way to get operator?
    let private getOperator tokensList = 
        let skip skipFunc =
            List.takeWhile skipFunc tokensList |> List.rev |> List.head
        skip 

    let private parsePrimary list =
        let firstSymbol = List.head list
        match firstSymbol.Type with
        | TokenType.True -> Literal True
        | TokenType.False -> Literal False
        | TokenType.Nil -> Literal Nil
        | TokenType.Number
        | TokenType.String -> Literal firstSymbol.Lexeme
        // TODO: Handle grouping
        

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

    let private binaryParsingFunc tokensList leftParsingFunc tokensToSkip =
        let left = leftParsingFunc tokensList
        let skipFunc = matchToken tokensToSkip
        let operator = getOperator tokensList skipFunc
        let right = leftParsingFunc (List.skipWhile skipFunc tokensList)
        
        Binary (left, operator, right)   

    let private parseFactor tokensList =
        binaryParsingFunc tokensList parseUnary [|TokenType.Slash; TokenType.Star|] 
    
    let private parseTerm tokensList =
        binaryParsingFunc tokensList parseFactor [|TokenType.Minus; TokenType.Plus|] 

    let private parseComparison tokensList =
        binaryParsingFunc tokensList parseTerm [|TokenType.Greater; TokenType.GreaterEqual; TokenType.Less; TokenType.LessEqual|]
    
    let private parseEquality tokensList =
        binaryParsingFunc tokensList parseComparison [|TokenType.BangEqual; TokenType.EqualEqual|] 
    
    let private parseExpression tokensList = parseEquality tokensList