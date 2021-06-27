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
    
    let raiseParserError token errorMessage =
        Common.reportTokenError token errorMessage
        raise <| InterpreterException (token.Line, token.Lexeme, errorMessage)
    
    let private consume (tokens : Token array) tokenType errorMessage =
        let currentToken = tokens.[currentTokenPosition]
        if matchTokenType currentToken [|tokenType|] then
            advance tokens
        else
            raiseParserError currentToken errorMessage
        
    // Possible bugs' nest. Revise.
    let rec private parsePrimary (tokens : Token array) =
        let currentToken = tokens.[currentTokenPosition]
        if (matchTokenType currentToken [|TokenType.Number; TokenType.String|]) then
            Literal (previous tokens).Lexeme
        elif (matchTokenType currentToken [|TokenType.LeftParen|]) then
            let insideExpression = parseExpression tokens
            consume tokens TokenType.RightParen "Expected ')' after expression" |> ignore 
            Grouping insideExpression
        elif (matchTokenType currentToken [|TokenType.True|]) then
            Literal True
        else
            Literal False
    and parseExpression tokens = parseEquality tokens
    and parseEquality tokens = 
        parseBinary tokens parseComparison [| TokenType.BangEqual; TokenType.EqualEqual |]
    and parseBinary tokens parseSideFunc tokenTypesToMatch = 
        let left = parseSideFunc tokens
        let skippedTokens =
            Array.skip currentTokenPosition tokens
            |> Array.takeWhile (fun t -> matchTokenType t tokenTypesToMatch)
        let operator = Array.last skippedTokens
        let right = parseSideFunc tokens
        Binary (left, operator, right)
    and parseComparison tokens =
        parseBinary tokens parseTerm [| TokenType.Greater; TokenType.GreaterEqual; TokenType.Less; TokenType.LessEqual |]
    and parseTerm tokens =
        parseBinary tokens parseFactor [| TokenType.Minus; TokenType.Plus |]
    and parseFactor tokens =
        parseBinary tokens parseUnary [| TokenType.Slash; TokenType.Star |]
    and parseUnary (tokens : Token array) =
        if matchTokenType tokens.[currentTokenPosition] [|TokenType.Bang; TokenType.Minus|] then
            let operator = previous tokens
            let right = parseUnary tokens
            Unary (operator, right)
        else
            parsePrimary tokens

    