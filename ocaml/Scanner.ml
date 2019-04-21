open Token

module Scanner = struct

  type scanner_context = {
    start: int;
    current: int;
    line: int;
    current_character: char option;
    source: string;
    tokens: Token.token list;
  }

  let make_token ctx token_type text literal = {ctx with tokens =
    List.append ctx.tokens [Token.TokenRecord {
      literal =  literal;
      lexeme = text;
      line = ctx.line;
      token_type = token_type;
    }]
  }

  let advance ctx =
    {ctx with current = ctx.current + 1; current_character = (
      try Some (String.get ctx.source ctx.current) with Invalid_argument s -> None
    )}

  let add_token context token_type literal =
    print_int (context.start);
    print_int context.current;
    let text = (String.sub context.source context.start context.current) in
    make_token context token_type text literal

  (* helper for end of recursion in scan_tokens too *)
  let add_EOF_token ctx = add_token ctx Token.EOF (Some Token.IDENTIFIER)

  let scan_token context =
    let ctx = advance context in
    match ctx.current_character with
    | None     -> add_EOF_token ctx
    | Some '(' -> add_token ctx Token.LEFT_PAREN (Some Token.IDENTIFIER)
    | Some ')' -> add_token ctx Token.RIGHT_PAREN (Some Token.IDENTIFIER)
    | Some '{' -> add_token ctx Token.LEFT_BRACE (Some Token.IDENTIFIER)
    | Some '}' -> add_token ctx Token.RIGHT_BRACE (Some Token.IDENTIFIER)
    | Some ',' -> add_token ctx Token.COMMA (Some Token.IDENTIFIER)
    | Some '.' -> add_token ctx Token.DOT (Some Token.IDENTIFIER)
    | Some '-' -> add_token ctx Token.MINUS (Some Token.IDENTIFIER)
    | Some '+' -> add_token ctx Token.PLUS (Some Token.IDENTIFIER)
    | Some ';' -> add_token ctx Token.SEMICOLON (Some Token.IDENTIFIER)
    | Some '*' -> add_token ctx Token.STAR (Some Token.IDENTIFIER)
    | _        -> ctx


  let is_at_end ctx = ctx.current >= String.length ctx.source

  let scan_tokens context =
    let rec scan ctx =
      let at_end = is_at_end ctx in
      match at_end with
      | true -> add_EOF_token ctx
      | false ->
        let new_ctx =
          {ctx with start = ctx.current} in
        let c = scan_token new_ctx in
        scan c
    in scan context

end