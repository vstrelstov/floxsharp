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
        
    let mutable private currentTokenPosition = 0
    
    let private previous (tokens : Token array) = tokens.[(currentTokenPosition - 1)]
          
    let private matchTokenType token =
        let matchType typesToMatch =
            let isMatch = Array.contains token.Type typesToMatch
            if isMatch then currentTokenPosition <- currentTokenPosition + 1
            isMatch
            
        matchType
        
    let private isAtEnd (tokens : Token array) =
        matchTokenType tokens.[currentTokenPosition] [|TokenType.EOF|] || currentTokenPosition >= (Array.length tokens)
        
    let private advance (tokens : Token array) =
        if not (isAtEnd tokens) then
            currentTokenPosition <- currentTokenPosition + 1
        previous tokens
        
    
        
    let parsePrimary tokens = Literal null  //TODO:  Stub to be implemented later
    
    let rec private parseUnary (tokens : Token array) =
        if matchTokenType tokens.[currentTokenPosition] [|TokenType.Bang; TokenType.Minus|] then
            let operator = previous tokens
            let right = parseUnary tokens
            Unary (operator, right)
        else
            parsePrimary tokens
        
    // TODO: Refactor
    let private parseBinary tokens parseSideFunc tokenTypesToMatch = 
        let left = parseSideFunc tokens
        let taken = Array.skip currentTokenPosition tokens |> Array.takeWhile (fun t -> matchTokenType t tokenTypesToMatch) // TODO: Rename
        let operator = Array.last taken
        let right = parseSideFunc tokens
        Binary (left, operator, right)
            
    let parseFactor tokens =
        parseBinary tokens parseUnary [| TokenType.Slash; TokenType.Star |]
        
    let parseTerm tokens =
        parseBinary tokens parseFactor [| TokenType.Minus; TokenType.Plus |]
        
    let parseComparison tokens =
        parseBinary tokens parseTerm [| TokenType.Greater; TokenType.GreaterEqual; TokenType.Less; TokenType.LessEqual |]
        
    let parseEquality tokens = 
        parseBinary tokens parseComparison [| TokenType.BangEqual; TokenType.EqualEqual |]
        
    let parseExpression tokens = parseEquality tokens
    