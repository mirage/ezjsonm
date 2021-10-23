open Test_helper
(* Check we're compatible with sexplib *)
type test = {
  foo: Ezjsonm.value;
  bar: Ezjsonm.t;
} [@@deriving sexp]

type 'a x = {
  to_json: 'a -> Ezjsonm.value;
  of_json: Ezjsonm.value -> 'a;
  test   : 'a Alcotest.testable;
}

let test x t =
  let j =  x.to_json t in
  let t' = x.of_json j in
  Alcotest.(check x.test) "idempotent JSON conversion" t t';
  let str = Ezjsonm.(value_to_string j) in
  let j' = Ezjsonm.(value_from_string str) in
  let t' = x.of_json j' in
  Alcotest.(check x.test) "idempotent string conversion" t t'

let string =
  { to_json = Ezjsonm.string;
    of_json = Ezjsonm.get_string;
    test    = Alcotest.string; },
  [""; "foo"; random_string 1024]

let list =
  { to_json = Ezjsonm.(list string);
    of_json = Ezjsonm.(get_list get_string);
    test    = Alcotest.(list string); },
  [
    random_list 30 random_string;
    random_list 10 (fun _ -> "foo");
    random_list 10000 (fun _ -> random_string 10);
  ]

let dict =
  { to_json = Ezjsonm.(dict);
    of_json = Ezjsonm.(get_dict);
    test    = Alcotest.(list (pair string json_v)); },
  [
    random_list 10000 (fun _ -> random_string 20, Ezjsonm.strings (random_list 10 random_string));
  ]

let deeply_nested =
  { to_json = (fun x -> x)
  ; of_json = (fun x -> x)
  ; test = Alcotest.testable (fun ppf x -> Fmt.pf ppf "%s" (Ezjsonm.value_to_string x)) (fun x y -> x = y) },
  let rec deep_list n v =
    match n with
    | 0 -> v
    | n -> deep_list (n - 1) (Ezjsonm.list (fun x -> x) [v])
  in
  let rec deep_dict n v =
    match n with
    | 0 -> v
    | n -> deep_dict (n - 1) (Ezjsonm.dict [ ("name", v) ])
  in
  [
    deep_list 10_000 (Ezjsonm.bool true);
    deep_dict 10_000 (Ezjsonm.bool true);
  ]

let tests t ts () = List.iter (test t) ts

let test_sexp_of_t () =
  let t = `A [ `String "hello" ] in
  let open Ezjsonm in
  let t'' = t_of_sexp @@ sexp_of_t t in
  Alcotest.(check json_t) "sexp_of_t" t t''

let test_sexp_of_value () =
  let v = `A [ `String "hello" ] in
  let open Ezjsonm in
  let v'' = value_of_sexp @@ sexp_of_value v in
  Alcotest.(check json_v) "sexp_of_value" v v''

module Errors = struct
  let pp_loc ppf ((a, b), (c, d)) =
    Format.fprintf ppf "%d:%d...%d:%d" a b c d

  let pp_read_error ppf = function
    | `Error (loc, err) ->
      Format.fprintf ppf "`Error at %a: %a" pp_loc loc Jsonm.pp_error err
    | `Unexpected `End_of_input ->
      Format.fprintf ppf "Unexpected end of input"
    | `Unexpected (`Lexeme (loc, lexeme, msg)) ->
      Format.fprintf ppf "Unexpected lexeme %a at %a: %s"
        Jsonm.pp_lexeme lexeme pp_loc loc msg
    | `Not_a_t _v ->
      Format.fprintf ppf "Expected an array or object"

  let read_error = Alcotest.of_pp pp_read_error
  let read_result = Alcotest.result json_v read_error

  let test_json_empty () =
    Alcotest.check' read_result ~msg:"empty input"
      ~expected:(Error (`Error (((1, 0), (1, 0)), `Expected `Json)))
      ~actual:(Ezjsonm.from_string_result "")

  let test_json_parse_error () =
    Alcotest.check' read_result ~msg:"parse error"
      ~expected:(Error (`Error (((2, 2), (2, 3)), `Illegal_number "1;")))
      ~actual:(Ezjsonm.from_string_result
              "[\n\
               \ 1;\n\
               \ 2\n\
               ]")

  let test_json_not_a_value () =
    Alcotest.check' read_result ~msg:"not a value"
      ~expected:(Error (`Not_a_t (`Float 42.)))
      ~actual:(Ezjsonm.from_string_result "42")
end

let () =
  let suite k (t, ts) = k, ["test", `Quick, tests t ts] in
  Alcotest.run "ezjsonm" [
    suite "string" string;
    suite "list"   list;
    suite "dict"   dict;
    suite "nested" deeply_nested;
    "sexp", [
      "sexp_of_t", `Quick, test_sexp_of_t;
      "sexp_of_value", `Quick, test_sexp_of_value;
    ];
    "errors", [
      "error: empty", `Quick, Errors.test_json_empty;
      "error: parse error", `Quick, Errors.test_json_parse_error;
      "error: not a value", `Quick, Errors.test_json_not_a_value;
    ];
  ]
