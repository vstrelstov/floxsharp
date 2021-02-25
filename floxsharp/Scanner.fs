namespace Floxsharp.Interpreter

open System
open Floxsharp.Common
open Floxsharp.Interpreter.Scanning

module Scanner =
    let private ignoredSymbols = [|' '; '\r'; '\t'|]
    let private doubleTokens = [|TokenType.GreaterEqual; TokenType.LessEqual; TokenType.EqualEqual; TokenType.BangEqual|]
    let private keywords = Map [
        ("and", TokenType.And); 
        ("class", TokenType.Class); 
        ("else", TokenType.Else); 
        ("false", TokenType.False);
        ("for", TokenType.For); 
        ("fun", TokenType.Fun); 
        ("if", TokenType.If); 
        ("nil", TokenType.Nil); 
        ("or", TokenType.Or);
        ("print", TokenType.Print); 
        ("return", TokenType.Return); 
        ("super", TokenType.Super); 
        ("this", TokenType.This);
        ("true", TokenType.True); 
        ("var", TokenType.Var); 
        ("while", TokenType.While)]

    let private getLexeme src skipFunction = 
        src
        |> List.takeWhile skipFunction
        |> List.map (fun c -> c.ToString())
        |> String.concat ""
        |> (fun res -> res.TrimStart '"')

    let scanTokens (source: string) =
        let mutable lineNumber = 1
    
        let createLexemedToken tokenType lexeme = 
            { Type = tokenType; Lexeme = lexeme; Line = lineNumber }
    
        let createToken tokenType = createLexemedToken tokenType String.Empty
                
        let rec loop source tokens = 
            let matchNext expected = Common.peekNextSymbol source = (Some expected)
    
            let createTokenByNextSymbol expectedSymbol matchTokenType mismatchTokenType =
                if matchNext expectedSymbol then
                    createToken matchTokenType
                else createToken mismatchTokenType
    
            let createStringToken = 
                let lexeme = getLexeme (Common.tryTail source) (fun c -> 
                    if c = '\n' then 
                        lineNumber <- (lineNumber + 1)
                    c <> '"')
                createLexemedToken TokenType.String lexeme
    
            let createNumberToken =
                let skipFunc = fun c -> Char.IsDigit c
                let integer = getLexeme source skipFunc
                let afterSkip = List.skip (String.length integer) source
                if peekNextSymbol afterSkip <> Some('.') then
                    createLexemedToken TokenType.Number integer
                else
                    let fractional = getLexeme afterSkip skipFunc
                    createLexemedToken TokenType.Number $"{integer}.{fractional}"
    
            let createIdentifierToken = 
                let skipFunc = fun c -> Char.IsLetterOrDigit c
                let lexeme = getLexeme source skipFunc
                if Map.containsKey (lexeme.ToLower()) keywords then
                    createLexemedToken keywords.[lexeme.ToLower()] lexeme
                else 
                    createLexemedToken TokenType.Identifier lexeme
    
            let scanToken currentChar =
                match currentChar with
                | '(' -> createToken TokenType.LeftParen
                | ')' -> createToken TokenType.RightParen
                | '{' -> createToken TokenType.LeftBrace
                | '}' -> createToken TokenType.RightBrace
                | ',' -> createToken TokenType.Comma
                | '.' -> createToken TokenType.Dot
                | '-' -> createToken TokenType.Minus
                | '+' -> createToken TokenType.Plus
                | ';' -> createToken TokenType.Semicolon
                | '*' -> createToken TokenType.Star
                | '/' -> createTokenByNextSymbol '/' TokenType.Comment TokenType.Slash
                | '!' -> createTokenByNextSymbol '=' TokenType.BangEqual TokenType.Bang
                | '=' -> createTokenByNextSymbol '=' TokenType.EqualEqual TokenType.Equal
                | '<' -> createTokenByNextSymbol '=' TokenType.LessEqual TokenType.Less
                | '>' -> createTokenByNextSymbol '=' TokenType.GreaterEqual TokenType.Greater
                | '"' -> createStringToken
                | _ ->
                    if Char.IsDigit currentChar then
                        createNumberToken
                    elif Char.IsLetter currentChar then 
                        createIdentifierToken
                    else raise <| Common.InterpreterException (lineNumber, String.Empty, "Unexpected character")

            let postprocessStringToken src lexeme = 
                let newSource = List.skip (String.length lexeme) src
                let newHead = List.tryHead newSource
                if Option.isNone newHead || newHead.Value <> '"' then
                    raise <| Common.InterpreterException (lineNumber, String.Empty, "Unterminated string")
                else Common.tryTail newSource
    
            match source with // TODO: Looks messed up and requires refactoring
            | [] -> tokens
            | head::tail when Array.contains head ignoredSymbols -> loop tail tokens
            | '\n'::tail ->
                lineNumber <- (lineNumber + 1)
                loop tail tokens
            | head::tail -> 
                let newToken = scanToken head
                match newToken.Type with
                | x when Array.contains x doubleTokens -> loop (Common.tryTail tail) (tokens @ [newToken])
                | TokenType.String -> loop (postprocessStringToken tail newToken.Lexeme) (tokens @ [newToken])
                | TokenType.Comment -> loop (List.skipWhile (fun c -> c <> '\n') tail) (tokens @ [newToken])
                | TokenType.Number
                | TokenType.Identifier -> loop (List.skip (String.length newToken.Lexeme) source) (tokens @ [newToken])
                | _ when Map.containsKey newToken.Lexeme keywords -> 
                    loop (List.skip (String.length newToken.Lexeme) source) (tokens @ [newToken])
                | _ -> loop tail (tokens @ [newToken])
    
        loop (source |> Seq.toList) []
