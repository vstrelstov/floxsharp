namespace floxsharp.Tests

open Floxsharp.Common
open Floxsharp.Interpreter
open Floxsharp.Interpreter.Scanning
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ScannerTests () =

    let getTokenTypes src =
        Scanner.scanTokens src |> List.map (fun token -> token.Type)

    let getTokenTypesAndLexemes src = 
        Scanner.scanTokens src |> List.map (fun token -> (token.Type, token.Lexeme))

    [<TestMethod>]
    member this.ScanSingleTokens () =
        let source = "({*-+.;},"
        let actual = getTokenTypes source
        let expected = [TokenType.LeftParen; TokenType.LeftBrace; TokenType.Star; TokenType.Minus; TokenType.Plus; TokenType.Dot; TokenType.Semicolon; TokenType.RightBrace; TokenType.Comma]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.ScanDoubleTokens () =
        let source = ">= <= !=       =="
        let actual = getTokenTypes source
        let expected = [TokenType.GreaterEqual; TokenType.LessEqual; TokenType.BangEqual; TokenType.EqualEqual]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.ScanSingleAndDoubleTokens () =
        let source = "=== =<="
        let actual = getTokenTypes source
        let expected = [TokenType.EqualEqual; TokenType.Equal; TokenType.Equal; TokenType.LessEqual]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.ScanSingleComment () = 
        let source = "//var a = 5;"
        let actual = getTokenTypes source
        Assert.AreEqual(actual, [TokenType.Comment])

    [<TestMethod>]
    member this.ScanString () =
        let source = "\"asdf\"\"qwer\""
        let actual = getTokenTypesAndLexemes source
        let expected = [(TokenType.String, "asdf"); (TokenType.String, "qwer")]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.ScanKeywords () =
        let source = "var while nil    or and for//true"
        let actual = getTokenTypes source
        let expected = [TokenType.Var; TokenType.While; TokenType.Nil; TokenType.Or; TokenType.And; TokenType.For; TokenType.Comment]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    member this.ScanExpression () = 
        let source = "var nullValue = nil; if nullValue == nil print;"
        let actual = getTokenTypesAndLexemes source
        let expected = [(TokenType.Var, "var"); (TokenType.Identifier, "nullValue"); (TokenType.Equal, ""); 
            (TokenType.Nil, "nil"); (TokenType.Semicolon, ""); (TokenType.If, "if"); (TokenType.Identifier, "nullValue");
            (TokenType.EqualEqual, ""); (TokenType.Nil, "nil"); (TokenType.Print, "print"); (TokenType.Semicolon, "")]
        Assert.AreEqual(actual, expected)

    [<TestMethod>]
    [<ExpectedException(typeof<Common.InterpreterException>)>]
    member this.ScanUnexpectedSymbol () = 
        let source = "var a = &&&"
        getTokenTypes source |> ignore