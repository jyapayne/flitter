open Common
open OUnit
open Generate_nim

module Ast = Ast_cpp
module Flag = Flag_parsing_cpp

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let _gen_nim file =
  Common.save_excursion Flag.error_recovery false (fun () ->
  Common.save_excursion Flag.show_parsing_error false (fun () ->
  Common.save_excursion Flag.verbose_parsing false (fun () ->
    Generate_nim.generate_nim file ~macro_files:[]
  )))

let strip_string s =
  let rep = Str.global_replace (Str.regexp "[\r\n\t ]$") "" s in
  Str.global_replace (Str.regexp "^[\r\n\t ]") "" rep

let get_files glob =
  try
    let path = Filename.concat Config_flitter.path "/tests/generators/nim/" in
    sort (Common2.glob (spf "%s/%s" path glob))
  with
    Common2.CmdError (a, b) -> []

let basename fpath =
  let fullpath = Filename.concat Config_flitter.path "/tests/generators/nim//" in
  readable ~root:fullpath fpath

let get_code_pairs name =
  let cglob = name ^ "*.c" in
  let cppglob = name ^ "*.cpp" in
  let nimglob = name ^ "*.nim" in
  let cfiles = get_files cglob @ get_files cppglob
  and nimfiles = get_files nimglob in
  List.map2 (fun cfile nimfile ->
      let test_code = _gen_nim cfile in
      let correct_code = Common.read_file nimfile in
      ((cfile, test_code), (nimfile, correct_code))
    )
    cfiles nimfiles

let gen_pair_test suite_name ((cfile, test_code), (nim_file, correct_code)) =
  "Test " ^ suite_name ^ ": " ^ (basename cfile) >:: (fun () ->
      let res = compare (strip_string test_code) (strip_string correct_code) == 0 in
      assert_bool ("Func is wrong: \n\nGenerated:\n" ^ test_code ^
                   "\n\nExpected:\n" ^ correct_code) res
    )

let tests suite_name test_file_name =
  let file_pairs = get_code_pairs test_file_name in
  List.map (gen_pair_test suite_name) file_pairs


let unittest =
  "generating_nim" >:::
    tests "function declarations" "test_func" @
    tests "enums" "test_enum" @
    tests "macros" "test_macro" @
    tests "operators" "test_operator" @
    tests "expressions" "test_expression" @
    tests "arrays" "test_array" @
    tests "structs" "test_struct" @
    tests "casting" "test_cast" @
    tests "conditionals" "test_conditional" @
    tests "vars" "test_var" @
    []
