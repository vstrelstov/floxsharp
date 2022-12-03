namespace floxsharp.Tests

open Floxsharp.Interpreter
open Floxsharp.Interpreter.Scanning
open Floxsharp.Interpreter.Parsing
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type PrettyPrinterTests () =

    [<TestMethod>]
    member this.PrettyPrintSimpleExpression () =
        let sampleExpression = Binary (Literal 1, { Type = TokenType.Plus; Lexeme = "+"; Line = 1}, Literal 2)
        let actual = PrettyPrinter.prettyPrint sampleExpression
        let expected = "(+ 1 2)"
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.PrettyPrintExpression () =
        let sampleExpression = 
            Binary (Unary ({ Type = TokenType.Minus; Lexeme = "-"; Line = 1; }, Literal 123),
                { Type = TokenType.Star; Lexeme = "*"; Line = 1},
                Grouping (Literal 45.67))
        let actual = PrettyPrinter.prettyPrint sampleExpression
        let expected = "(* (- 123) (group 45.67))"
        Assert.AreEqual(expected, actual)
