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

  let is_at_end ctx = ctx.current >= String.length ctx.source

  (** [match_sign expected ctx] returns tuple of (boolean, scanner_context)
    returns (false, ctx) if is at end
    returns (false, ctx) if character of source at current (String.get source current) isn't expected
    returns (true, ctx) and increments current otherwise
   **)

  let match_sign expected ctx =
    let char_at_end = (String.get ctx.source ctx.current) = expected in
    let at_end = is_at_end ctx in
    match (at_end, char_at_end) with
    | (true, _) -> (false, ctx)
    | ( _, false) -> (false, ctx)
    | (_, _) -> (true, {ctx with current = ctx.current + 1})

  let peek ctx =
    if is_at_end ctx then None else Some (String.get ctx.source ctx.current)

  let peek_next ctx =
    if (ctx.current + 1 > (String.length ctx.source))
      then None
      else Some (String.get ctx.source (ctx.current + 1))

  let advance_line ctx = {ctx with line = ctx.line + 1}

  let rec find_comment ctx =
    if (peek ctx != Some '\n') && not (is_at_end ctx)
      then find_comment (advance ctx)
      else ctx

  let rec string_literal ctx =
    let next_is_quote = (peek ctx) = Some '"' in
    let at_end = is_at_end ctx in
    let advance_line_if_newline ctx =
      if peek ctx = Some '\n' then advance_line ctx else ctx in
    match (not next_is_quote, not at_end) with
      | (true, true) -> advance_line_if_newline ctx |> advance |> string_literal
      | (_, _) ->
        (* this gets spicy fast *)
        if is_at_end ctx
          then error ctx "Unterminated string"
          else let advanced_ctx = advance ctx in
          let string_value = (String.sub ctx.source (ctx.start + 1) (ctx.current - ctx.start)) in
          let literal = Some (Token.STRING_LITERAL string_value) in
          make_token Token.STRING string_value literal advanced_ctx

  let is_digit int_option = match int_option with
    | None -> false
    | Some c -> c >= '0' && c <= '9'

  let rec find_digits ctx =
    if is_digit (peek ctx) then ctx |> advance |> find_digits
    else ctx

  let find_fractional_digits ctx = match (peek ctx), (is_digit (peek_next ctx)) with
    | Some '.', true -> ctx |> advance |> find_digits
    | _ -> ctx

  let add_number_token ctx =
    let src = (String.sub ctx.source ctx.start (ctx.current - ctx.start)) in
    make_token Token.NUMBER src (Some (Token.NUMBER_LITERAL (float_of_string src))) ctx

  let add_number ctx = ctx |> find_digits |> find_fractional_digits |> add_number_token

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
    | Some '!' ->
      begin match (match_sign '=' ctx) with
        | (true, ctx') -> add_token Token.BANG_EQUAL (Some Token.IDENTIFIER) ctx'
        | (false, ctx') -> add_token Token.BANG (Some Token.IDENTIFIER) ctx'
      end
    | Some '=' ->
      begin match (match_sign '=' ctx) with
        | (true, ctx') -> add_token Token.EQUAL_EQUAL (Some Token.IDENTIFIER) ctx'
        | (false, ctx') -> add_token Token.EQUAL (Some Token.IDENTIFIER) ctx'
      end
    | Some '<' ->
      begin match (match_sign '=' ctx) with
        | (true, ctx') -> add_token Token.LESS_EQUAL (Some Token.IDENTIFIER) ctx'
        | (false, ctx') -> add_token Token.LESS (Some Token.IDENTIFIER) ctx'
      end
    | Some '>' ->
      begin match (match_sign '=' ctx) with
        | (true, ctx') -> add_token Token.GREATER_EQUAL (Some Token.IDENTIFIER) ctx'
        | (false, ctx') -> add_token Token.GREATER (Some Token.IDENTIFIER) ctx'
      end
    | Some '/' ->
        begin match (match_sign '/' ctx) with
        | (true, ctx') -> find_comment ctx'
        | (false, ctx') -> add_token Token.SLASH (Some Token.IDENTIFIER) ctx'
      end
    | Some ' ' -> ctx
    | Some '\r' -> ctx
    | Some '\t' -> ctx
    | Some '\n' -> advance_line ctx
    | Some a when (is_digit (Some a)) -> add_number ctx
    | _        -> error ctx "Unexpected character."

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