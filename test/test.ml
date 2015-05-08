open OUnit2

open Ezjsonm

let test_find test_ctxt = 
  let json = from_string "{ \"title\": \"A title\" }" in
  assert_equal (find json [ "title" ]) (`String "A title")

let suite = "suite" >:::
  ["find" >:: test_find]

let _ =
 run_test_tt_main suite
