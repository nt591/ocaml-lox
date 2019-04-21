open LoxRunner

module Lox = struct

  let run_prompt = fun _ ->
    while true do
      print_string "> ";
      let input = read_line ()
      in LoxRunner.run input;
    done

  let run_file arg =
    let ic = open_in arg in
    try
      (* read entire file *)
      let line = really_input_string ic (in_channel_length ic) in
      LoxRunner.run line;
      flush stdout;
      close_in ic
    with e ->
      close_in_noerr ic;
      raise e

  let main args = match List.length args with
    | 0 -> run_prompt ()
    | 1 -> run_file (List.hd args)
    | _ -> failwith "Usage: jlox [script]"
end