open Common

open Parse_info
open Ast_cpp
module Ast = Ast_cpp
module Flag = Flag_parsing_cpp
module TH = Token_helpers_cpp

module Stat = Parse_info

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_cpp file =
  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;
  let toks = Parse_cpp.tokens file in
  toks +> List.iter (fun x -> pr2_gen x);
  ()

let test_dump_cpp file =
  Parse_cpp.init_defs !Flag.macros_h;
  let ast = Parse_cpp.parse_program file in
  let v = Meta_ast_cpp.vof_program ast in
  let s = Ocaml.string_of_v v in
  pr s


let test_dump_cpp_full file =
  Parse_cpp.init_defs !Flag.macros_h;
  let ast = Parse_cpp.parse_program file in
  let toks = Parse_cpp.tokens file in
  let precision = { Meta_ast_generic.
     full_info = true; type_info = true; token_info = true;
  }
  in
  let v = Meta_ast_cpp.vof_program ~precision ast in
  let s = Ocaml.string_of_v v in
  pr s;
  toks +> List.iter (fun tok ->
    match tok with
    | Parser_cpp.TComment (ii) ->
        let v = Parse_info.vof_info ii in
        let s = Ocaml.string_of_v v in
        pr s
    | _ -> ()
  );
  ()

let test_dump_cpp_view file =
  Parse_cpp.init_defs !Flag.macros_h;
  let toks_orig = Parse_cpp.tokens file in
  let toks =
    toks_orig +> Common.exclude (fun x ->
      Token_helpers_cpp.is_comment x ||
      Token_helpers_cpp.is_eof x
    )
  in
  let extended = toks +> List.map Token_views_cpp.mk_token_extended in
  Parsing_hacks_cpp.find_template_inf_sup extended;

  let multi = Token_views_cpp.mk_multi extended in
  Token_views_context.set_context_tag_multi multi;
  let v = Token_views_cpp.vof_multi_grouped_list multi in
  let s = Ocaml.string_of_v v in
  pr s


let test_parse_cpp_fuzzy xs =
  let fullxs = Lib_parsing_cpp.find_source_files_of_dir_or_files xs
    +> Skip_code.filter_files_if_skip_list
  in
  fullxs +> Console.progress (fun k -> List.iter (fun file ->
    k ();
    Common.save_excursion Flag_parsing_cpp.strict_lexer true (fun () ->
      try
        let _fuzzy = Parse_cpp.parse_fuzzy file in
        ()
      with exn ->
        pr2 (spf "PB with: %s, exn = %s" file (Common.exn_to_s exn));
    )
  ))

let test_dump_cpp_fuzzy file =
  let fuzzy, _toks = Parse_cpp.parse_fuzzy file in
  let v = Ast_fuzzy.vof_trees fuzzy in
  let s = Ocaml.string_of_v v in
  pr2 s

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)

let actions () = [
    "-tokens_cpp", "   <file>",
    Common.mk_action_1_arg test_tokens_cpp;

    "-dump_cpp", "   <file>",
    Common.mk_action_1_arg test_dump_cpp;

    "-dump_cpp_full", "   <file>",
    Common.mk_action_1_arg test_dump_cpp_full;
    "-dump_cpp_view", "   <file>",
    Common.mk_action_1_arg test_dump_cpp_view;

    "-parse_cpp_fuzzy", "   <files or dirs>",
    Common.mk_action_n_arg test_parse_cpp_fuzzy;
    "-dump_cpp_fuzzy", "   <file>",
    Common.mk_action_1_arg test_dump_cpp_fuzzy;

]
