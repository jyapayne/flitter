open OUnit
open Common2

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

(* In addition to flags that can be tweaked via -xxx options (cf the
 * full list of options in the "the options" section below), this
 * program also depends on external files ?
 *)
let verbose = ref false

(* action mode *)
let action = ref ""

let test regexp =

  (* There is no reflection in OCaml so the unit test framework OUnit requires
   * us to explicitely build the test suites (which is not that bad).
   *)
  let tests =
    "all" >::: [
      Unit_generate_nim.unittest;
    ]
  in
  let suite =
    if regexp = "all"
    then tests
    else
      let paths =
        OUnit.test_case_paths tests +> List.map OUnit.string_of_path in
      let keep = paths
        +> List.filter (fun path ->
          pr2 path;
          path =~ (".*" ^ regexp))
      in
      Common2.some (OUnit.test_filter keep tests)
  in

  let results = OUnit.run_test_tt ~verbose:!verbose suite in
  let has_an_error =
    results +> List.exists (function
    | OUnit.RSuccess _ | OUnit.RSkip _ | OUnit.RTodo _ -> false
    | OUnit.RFailure _ | OUnit.RError _ -> true
    )
  in
  raise (Common.UnixExit (if has_an_error then 1 else 0))

let main_action x =
  test x



let all_actions () =
  []

let options () = [
  "-verbose", Arg.Set verbose,
  " ";
  ] @
  Common.options_of_actions action (all_actions()) @
  Common2.cmdline_flags_devel () @
  Common2.cmdline_flags_other () @
  [
    "-version",   Arg.Unit (fun () ->
      pr2 (spf "flitter (test) version: %s" Config_flitter.version);
      exit 0;
    ),
    "  guess what";
    (* this can not be factorized in Common *)
    "-date",   Arg.Unit (fun () ->
      pr2 "version: $Date: 2008/10/26 00:44:57 $";
      raise (Common.UnixExit 0)
    ),
    "   guess what";
  ]

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () =

  Gc.set {(Gc.get ()) with Gc.stack_limit = 1000 * 1024 * 1024};

  let usage_msg =
    "Usage: " ^ Common2.basename Sys.argv.(0) ^
      " [options] <file or dir> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () ->

    (match args with

    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) ->
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) ->
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | [x] ->
        main_action x

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | _ ->
        Common.usage usage_msg (options());
        failwith "too few or too many arguments"
    )
  )

(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () ->
    main ();
  )
