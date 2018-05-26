open Common
open OUnit
open Generate_nim


let unittest =
  "generating_nim" >::: [
    "Test function declarations" >:: (fun () ->
        pr "None"
      )
  ]
