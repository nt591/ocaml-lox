module  Lox : sig
  val run_prompt : string -> unit
  val run_file : string -> unit
  val run : string -> unit
  val error : string -> string -> string

end