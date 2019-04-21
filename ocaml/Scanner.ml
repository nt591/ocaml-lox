open Token

module Scanner = struct

  type scanner_context = {
    start: int;
    current: int;
    line: int;
    current_character: char option;
    source: string;
    tokens: Token.token list;
    had_error: bool;
  }


  let report ctx where message =
    print_endline (String.concat "" ["[line "; (string_of_int ctx.line); "] Error"; where; ": "; message]);
    {ctx with had_error = true}

  let error ctx message = report ctx "" message

  let make_token token_type text literal ctx = {ctx with tokens =
    Token.TokenRecord {
      literal =  literal;
      lexeme = text;
      line = ctx.line;
      token_type = token_type;
    } :: ctx.tokens
  }

  let advance ctx =
    {ctx with current = ctx.current + 1; current_character = (
      try Some (String.get ctx.source ctx.current) with Invalid_argument _ -> None
    )}

  let add_token token_type literal context =
    let substr ctx = String.sub ctx.source ctx.start (ctx.current - ctx.start) in
    let text = substr context in
    make_token token_type text literal context

  (* helper for end of recursion in scan_tokens too *)
  let add_EOF_token ctx = add_token Token.EOF (Some Token.IDENTIFIER) ctx

  let scan_token context =
    let ctx = advance context in
    match ctx.current_character with
    | None     -> add_EOF_token ctx
    | Some '(' -> add_token Token.LEFT_PAREN (Some Token.IDENTIFIER) ctx
    | Some ')' -> add_token Token.RIGHT_PAREN (Some Token.IDENTIFIER) ctx
    | Some '{' -> add_token Token.LEFT_BRACE (Some Token.IDENTIFIER) ctx
    | Some '}' -> add_token Token.RIGHT_BRACE (Some Token.IDENTIFIER) ctx
    | Some ',' -> add_token Token.COMMA (Some Token.IDENTIFIER) ctx
    | Some '.' -> add_token Token.DOT (Some Token.IDENTIFIER) ctx
    | Some '-' -> add_token Token.MINUS (Some Token.IDENTIFIER) ctx
    | Some '+' -> add_token Token.PLUS (Some Token.IDENTIFIER) ctx
    | Some ';' -> add_token Token.SEMICOLON (Some Token.IDENTIFIER) ctx
    | Some '*' -> add_token Token.STAR (Some Token.IDENTIFIER) ctx
    | _        -> error ctx "Unexpected character."


  let is_at_end ctx = ctx.current >= String.length ctx.source

  let reverse_tokens ctx = {
    ctx with tokens = List.rev ctx.tokens
  }

  let _scan_tokens context =
    let rec scan ctx =
      let at_end = is_at_end ctx in
      match at_end with
      | true -> ctx |> add_EOF_token |> reverse_tokens
      | false ->
        let new_ctx =
          {ctx with start = ctx.current} in
        let c = scan_token new_ctx in
        scan c
    in scan context

  let scan_tokens source =
    let ctx = {
      start = 0;
      line = 1;
      current = 1;
      current_character = None;
      source = source;
      tokens = [];
      had_error = false;
    } in
    _scan_tokens ctx

end