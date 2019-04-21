open Scanner

module LoxRunner = struct
  let run (source : string) =
    let rec _run (ctx : Scanner.scanner_context) =
      match (ctx.tokens) with
      | [] -> ()
      | h::t -> match h with
        (* for some reason needed to match on type to get out the type *)
       | TokenRecord a -> print_endline a.lexeme; _run {ctx with tokens = t}
    in _run (Scanner.scan_tokens source)
end