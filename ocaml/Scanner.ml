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
      try Some (String.get ctx.source ctx.current) with Invalid_argument s -> None
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
    | _        -> ctx


  let is_at_end ctx = ctx.current >= String.length ctx.source

  let reverse_tokens ctx = {
    ctx with tokens = List.rev ctx.tokens
  }

  let scan_tokens context =
    let rec scan ctx =
      let at_end = is_at_end ctx in
      print_int ctx.start;
      print_endline " START ";
      print_int ctx.current;
      print_endline " CURRENT ";
      print_endline ("AT_END " ^ string_of_bool at_end);
      match at_end with
      | true -> ctx |> add_EOF_token |> reverse_tokens
      | false ->
        let new_ctx =
          {ctx with start = ctx.current} in
        let c = scan_token new_ctx in
        scan c
    in scan context

end