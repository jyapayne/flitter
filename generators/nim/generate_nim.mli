val test_gen_nim :
  Common.filename -> unit

val generate_nim :
  Common.filename -> ?macro_files:Common.filename list -> string

val actions : unit -> Common.cmdline_actions
