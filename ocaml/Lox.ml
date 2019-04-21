open Scanner

module Lox = struct
  (* todo: stop this *)
  let had_error = ref false

  let run (source : string) =
    let rec _run tokens =
      match (tokens) with
      | [] -> ()
      | h::t -> print_endline h; _run t
    in _run (Scanner.scan_tokens source)

  let run_prompt = fun _ ->
    while true do
      print_string "> ";
      let input = read_line ()
      in run input;
      had_error := false
    done

  let run_file arg =
    match !had_error with
    | true -> exit 65
    | false ->
      let ic = open_in arg in
      try
        (* read entire file *)
        let line = really_input_string ic (in_channel_length ic) in
        run line;
        flush stdout;
        close_in ic
      with e ->
        close_in_noerr ic;
        raise e

  let report line where message =
    had_error := true;
    String.concat "" ["[line "; line; "] Error"; where; ": "; message]

  let error line message = report line "" message


  let main args = match List.length args with
    | 0 -> run_prompt ()
    | 1 -> run_file (List.hd args)
    | _ -> failwith "Usage: jlox [script]"
end