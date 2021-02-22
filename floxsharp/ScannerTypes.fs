namespace Floxsharp.Interpreter.Scanning

[<AutoOpen>]
module ScanerTypes = 
    type TokenType = 
        | LeftParen
        | RightParen
        | LeftBrace
        | RightBrace
        | Comma
        | Dot
        | Minus
        | Plus
        | Semicolon
        | Slash
        | Star
        | Bang
        | BangEqual
        | Equal
        | EqualEqual
        | Greater
        | GreaterEqual
        | Less
        | LessEqual
        | Comment
        | Identifier
        | String
        | Number
        | And
        | Class
        | Else
        | False
        | Fun
        | For
        | If
        | Nil
        | Or
        | Print
        | Return 
        | Super
        | This
        | True
        | Var
        | While
        | EOF
    
    type Token =
        {
            Type: TokenType;
            Lexeme: string;
            Line: int;
        }

    