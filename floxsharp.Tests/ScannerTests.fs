namespace floxsharp.Tests

open Floxsharp.Common
open Floxsharp.Interpreter
open Floxsharp.Interpreter.Scanning
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type ScannerTests() =

    let getTokenTypes src =
        Scanner.scanTokens src
        |> List.map (fun token -> token.Type)

    let getTokenTypesAndLexemes src =
        Scanner.scanTokens src
        |> List.map (fun token -> (token.Type, token.Lexeme))

    [<TestMethod>]
    member this.ScanEmptySource() =
        Assert.AreEqual([ TokenType.EOF ], getTokenTypes "")

    [<TestMethod>]
    member this.ScanIgnoredSymbols() =
        Assert.AreEqual([ TokenType.EOF ], getTokenTypes "\n \t\r")

    [<TestMethod>]
    member this.ScanSingleTokens() =
        let source = "({*-+.;},"
        let actual = getTokenTypes source

        let expected =
            [ TokenType.LeftParen
              TokenType.LeftBrace
              TokenType.Star
              TokenType.Minus
              TokenType.Plus
              TokenType.Dot
              TokenType.Semicolon
              TokenType.RightBrace
              TokenType.Comma
              TokenType.EOF ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanDoubleTokens() =
        let source = ">= <= !=       =="
        let actual = getTokenTypes source

        let expected =
            [ TokenType.GreaterEqual
              TokenType.LessEqual
              TokenType.BangEqual
              TokenType.EqualEqual
              TokenType.EOF ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanSingleAndDoubleTokens() =
        let source = "=== =<="
        let actual = getTokenTypes source

        let expected =
            [ TokenType.EqualEqual
              TokenType.Equal
              TokenType.Equal
              TokenType.LessEqual
              TokenType.EOF ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanSingleComment() =
        let source = "//var a = 5;"
        let actual = getTokenTypes source
        Assert.AreEqual(actual, [ TokenType.Comment; TokenType.EOF ])

    [<TestMethod>]
    member this.ScanString() =
        let source = "\"asdf\nght\"\"qwer\""
        let actual = getTokenTypesAndLexemes source

        let expected =
            [ (TokenType.String, "asdf\nght")
              (TokenType.String, "qwer")
              (TokenType.EOF, "") ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanKeywords() =
        let source =
            "var while nil    or and for//true"

        let actual = getTokenTypes source

        let expected =
            [ TokenType.Var
              TokenType.While
              TokenType.Nil
              TokenType.Or
              TokenType.And
              TokenType.For
              TokenType.Comment
              TokenType.EOF ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanExpression() =
        let source =
            "var nullValue = nil; \n if nullValue == nil print;"

        let actual = getTokenTypesAndLexemes source

        let expected =
            [ (TokenType.Var, "var")
              (TokenType.Identifier, "nullValue")
              (TokenType.Equal, "")
              (TokenType.Nil, "nil")
              (TokenType.Semicolon, "")
              (TokenType.If, "if")
              (TokenType.Identifier, "nullValue")
              (TokenType.EqualEqual, "")
              (TokenType.Nil, "nil")
              (TokenType.Print, "print")
              (TokenType.Semicolon, "")
              (TokenType.EOF, "") ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanExpressionWithComment() =
        let source =
            "var str = \"abc\"; \n // this is a comment"

        let actual = getTokenTypesAndLexemes source

        let expected =
            [ (TokenType.Var, "var")
              (TokenType.Identifier, "str")
              (TokenType.Equal, "")
              (TokenType.String, "abc")
              (TokenType.Semicolon, "")
              (TokenType.Comment, "")
              (TokenType.EOF, "") ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.ScanExpressionWithUnseparatedKeywords() =
        let source = "vartruefalse"
        let actual = getTokenTypesAndLexemes source

        let expected =
            [ (TokenType.Identifier, "vartruefalse")
              (TokenType.EOF, "") ]

        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    [<ExpectedException(typeof<Common.InterpreterException>)>]
    member this.ScanUnexpectedSymbol() =
        let source = "var a = &&&"
        getTokenTypes source |> ignore

    [<TestMethod>]
    [<ExpectedException(typeof<InterpreterException>)>]
    member this.ScanUnterminatedString() =
        let source = "var string = \"asdf ;"
        getTokenTypes source |> ignore