
(* the token list contains also the comment-tokens *)
type toplevels_and_tokens = (Ast_cpp.toplevel * Parser_cpp.token list) list

(* actually covers Lexical, Parsing, and Semantic errors *)
exception Parse_error of Parse_info.info

(* This is the main function. It uses _defs below which often comes 
 * from a standard.h macro file. It will raise Parse_error unless
 * Flag_parsing_cpp.error_recovery is set.
 *)
val parse:
  Common.filename -> (toplevels_and_tokens * Parse_info.parsing_stat)

val parse_program:  
  Common.filename -> Ast_cpp.program
val parse_with_lang:
  ?lang:Flag_parsing_cpp.language ->
  Common.filename -> (toplevels_and_tokens * Parse_info.parsing_stat)

val parse_fuzzy:
  Common.filename -> Ast_fuzzy.tree list * Parser_cpp.token list

(* usually correspond to what is inside your macros.h *)
val _defs : (string, Pp_token.define_body) Hashtbl.t
val init_defs : Common.filename -> unit
val add_defs : Common.filename -> unit
(* used to extract macros from standard.h, but also now used on C files
 * in -extract_macros to assist in building a macros.h 
 *)
val extract_macros: 
  Common.filename -> (string, Pp_token.define_body) Common.assoc

(* usually correspond to what is inside your standard.h *)
(* val _defs_builtins : (string, Cpp_token_c.define_def) Hashtbl.t ref *)
(* todo: init_defs_macros and init_defs_builtins *)

(* subsystem testing *)
val tokens:      Common.filename -> Parser_cpp.token list

(* a few helpers *)
val program_of_program2: toplevels_and_tokens -> Ast_cpp.program

