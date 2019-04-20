open Token

module Scanner = struct

  let makeToken tokenType lit = Token.TokenRecord {
    literal =  lit;
    lexeme = "hello";
    line = 2;
    tokenType = tokenType;
  }

  let addToken tokenType = makeToken tokenType None

  let scanToken character =
    match character with
    | "(" -> addToken Token.LEFT_PAREN
    | ")" -> addToken Token.RIGHT_PAREN
    | "{" -> addToken Token.LEFT_BRACE
    | "}" -> addToken Token.RIGHT_BRACE
    | "," -> addToken Token.COMMA
    | "." -> addToken Token.DOT
    | "-" -> addToken Token.MINUS
    | "+" -> addToken Token.PLUS
    | ";" -> addToken Token.SEMICOLON
    | "*" -> addToken Token.STAR
    | _ -> addToken Token.EOF

  let scanTokens source = print_endline source
end