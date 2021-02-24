(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* From http://erratique.ch/software/jsonm/doc/Jsonm.html#datamodel *)
type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list ]

type t =
  [ `A of value list
  | `O of (string * value) list ]

let value: t -> value = fun t -> (t :> value)

module List = struct
  include List

  (* Tail-recursive List.map *)
  let map f l = rev (rev_map f l)
end

type error_location = (int * int) * (int * int)
type read_value_error = [
  | `Error of error_location * Jsonm.error
  | `Unexpected of [ `Lexeme of error_location * Jsonm.lexeme * string | `End_of_input ]
]
type read_error = [ read_value_error | `Not_a_t of value ]

exception Abort of read_value_error

let loc decoder = Jsonm.decoded_range decoder

let dec decoder = match Jsonm.decode decoder with
  | `Lexeme l -> l
  | `Error e -> raise (Abort (`Error (loc decoder, e)))
  | `End -> raise (Abort (`Unexpected `End_of_input))
  | `Await -> assert false

type value_ =
  | Value of Jsonm.lexeme
  | Arr of value list
  | Obj of (string * value) list
and atom = [ `Bool of bool | `Float of float | `Null | `String of string ]

let identity x = x

let is_jsoo = match Sys.backend_type with
  | Native | Bytecode -> false | _ -> true

let rec unroll_arr vs decoder =
  let v = ref `Ae in
  let res = ref vs in
  while v := dec decoder ; !v <> `Ae do
    res := loop decoder (Value !v) identity :: !res ;
  done ; `A (List.rev !res)

and loop decoder f k = match f with
  | Value l ->
    ( match l with
    | `Os -> loop decoder (Obj []) k
    | `As -> loop decoder (Arr []) k
    | #atom as v -> k (v :> value)
    | l -> raise (Abort (`Unexpected (`Lexeme (loc decoder, l, "value")))) )
  | Arr vs when is_jsoo ->
    k (unroll_arr vs decoder)
  | Arr vs ->
    ( match dec decoder with
    | `Ae -> k (`A (List.rev vs))
    | v ->
      loop decoder (Value v) (fun v -> loop decoder (Arr (v :: vs)) k) )
  | Obj ms ->
    ( match dec decoder with
    | `Oe -> k (`O (List.rev ms))
    | `Name n -> loop decoder (Value (dec decoder)) (fun v -> loop decoder (Obj ((n, v) :: ms)) k)
    | l -> raise (Abort (`Unexpected (`Lexeme (loc decoder, l, "object fields")))) )

let json_of_src src : (value, [> read_value_error]) result =
  let decoder = Jsonm.decoder src in
  try Ok (loop decoder (Value (dec decoder)) identity)
  with Abort (#read_value_error as err) -> Error err

let value_to_dst ?(minify=true) dst json =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec t v k e = match v with
    | `A vs -> arr vs k e
    | `O ms -> obj ms k e
  and value v k e = match v with
    | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
    | #t as x -> t (x :> t) k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
    | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify dst in
  let finish e = ignore (Jsonm.encode e `End) in
  value json finish e

let value_to_buffer ?minify buf json =
  value_to_dst ?minify (`Buffer buf) json

let to_buffer ?minify buf json = value_to_buffer ?minify buf (json :> value)

let value_to_string ?minify json =
  let buf = Buffer.create 1024 in
  value_to_buffer ?minify buf json;
  Buffer.contents buf

let to_string ?minify json = value_to_string ?minify (json :> value)

let value_to_channel ?minify oc json =
  value_to_dst ?minify (`Channel oc) json

let to_channel ?minify oc json = value_to_channel ?minify oc (json :> value)

exception Parse_error of value * string

let parse_error t fmt =
  Printf.kprintf (fun msg ->
      raise (Parse_error (t, msg))
    ) fmt

let wrap t = `A [t]

let unwrap = function
  | `A [t] -> t
  | v -> parse_error (v :> value) "Not unwrappable"

let read_error_description : [< read_error ] -> string = function
  | `Error (_loc, err) ->
    Format.asprintf "%a" Jsonm.pp_error err
  | `Unexpected `End_of_input ->
    Format.sprintf "Unexpected end of input"
  | `Unexpected (`Lexeme (_loc, _l, expectation)) ->
    Format.sprintf "Unexpected input when parsing a %s" expectation
  | `Not_a_t _value ->
    "We expected a well-formed JSON document (array or object)"

let read_error_location : [< read_error ] -> error_location option = function
  | `Error (loc, _) -> Some loc
  | `Unexpected `End_of_input -> None
  | `Unexpected (`Lexeme (loc, _l, _expectation)) -> Some loc
  | `Not_a_t _value -> None

let value_from_src_result src = json_of_src src

let value_from_src src =
  match value_from_src_result src with
  | Ok t      -> t
  | Error e -> parse_error `Null "JSON.of_buffer %s" (read_error_description e)

let value_from_string_result str = value_from_src_result (`String str)
let value_from_string str = value_from_src (`String str)

let value_from_channel_result chan = value_from_src_result (`Channel chan)
let value_from_channel chan = value_from_src (`Channel chan)

let ensure_document_result: [> value] -> ([> t], [> read_error]) result = function
  | #t as t -> Ok t
  | value -> Error (`Not_a_t value)

let ensure_document: [> value] -> [> t] = function
  | #t as t -> t
  | t -> raise (Parse_error (t, "not a valid JSON array/object"))

let from_string str = value_from_string str |> ensure_document
let from_channel chan = value_from_channel chan |> ensure_document

let from_string_result str =
  Result.bind (value_from_string_result str) ensure_document_result
let from_channel_result chan =
  Result.bind (value_from_channel_result chan) ensure_document_result

(* unit *)
let unit () = `Null

let get_unit = function
  | `Null  -> ()
  | j      -> parse_error j "Ezjsonm.get_unit"

(* bool *)
let bool b = `Bool b

let get_bool = function
  | `Bool b -> b
  | j       -> parse_error j "Ezjsonm.get_bool"

(* string *)
let string s = `String s

let get_string = function
  | `String s -> s
  | j         -> parse_error j "Ezjsonm.get_string"

(* int *)
let int i = `Float (float_of_int i)
let int32 i = `Float (Int32.to_float i)
let int64 i = `Float (Int64.to_float i)

let get_int = function
  | `Float f -> int_of_float f
  | j        -> parse_error j "Ezjsonm.get_int"

let get_int32 = function
  | `Float f -> Int32.of_float f
  | j        -> parse_error j "Ezjsonm.get_int32"

let get_int64 = function
  | `Float f -> Int64.of_float f
  | j        -> parse_error j "Ezjsonm.get_int64"

(* float *)
let float f = `Float f

let get_float = function
  | `Float f -> f
  | j        -> parse_error j "Ezjsonm.get_float"

(* list *)
let list fn l =
  `A (List.map fn l)

let get_list fn = function
  | `A ks -> List.map fn ks
  | j     -> parse_error j "Ezjsonm.get_list"

(* string lists *)
let strings strings = list string strings

let get_strings = get_list get_string

(* options *)
let option fn = function
  | None   -> `Null
  | Some x -> `A [fn x]

let get_option fn = function
  | `Null  -> None
  | `A [j] -> Some (fn j)
  | j -> parse_error j "Ezjsonm.get_option"

(* dict *)
let dict d = `O d

let get_dict = function
  | `O d -> d
  | j    -> parse_error j "Ezjsonm.get_dict"

(* pairs *)
let pair fk fv (k, v) =
  `A [fk k; fv v]

let get_pair fk fv = function
  | `A [k; v] -> (fk k, fv v)
  | j         -> parse_error j "Ezjsonm.get_pair"

(* triple *)

let triple fa fb fc (a, b, c) =
  `A [fa a; fb b; fc c]

let get_triple fa fb fc = function
  | `A [a; b; c] -> (fa a, fb b, fc c)
  | j -> parse_error j "Ezjsonm.get_triple"

let mem t path =
  let rec aux j p = match p, j with
    | []   , _    -> true
    | h::tl, `O o -> List.mem_assoc h o && aux (List.assoc h o) tl
    | _           -> false in
  aux t path

let find t path =
  let rec aux j p = match p, j with
    | []   , j    -> j
    | h::tl, `O o -> aux (List.assoc h o) tl
    | _           -> raise Not_found in
  aux t path

let find_opt t path =
  try Some (find t path) with Not_found -> None

let map_dict f dict label =
  let rec aux acc = function
    | []    ->
      begin match f `Null with
        | None   -> List.rev acc
        | Some j -> List.rev_append acc [label, j]
      end
    | (l,j) as e :: dict ->
      if l = label then
        match f j with
        | None   -> List.rev_append acc dict
        | Some j -> List.rev_append acc ((l,j)::dict)
      else
        aux (e::acc) dict in
  aux [] dict

let map f t path =
  let rec aux t = function
    | []    -> f t
    | h::tl ->
      match t with
      | `O d -> Some (`O (map_dict (fun t -> aux t tl) d h))
      | _    -> None in
  match aux t path with
  | None   -> raise Not_found
  | Some j -> j

let update t path v =
  map (fun _ -> v) t path

exception Not_utf8

let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8 (fun _ _ -> function
        | `Malformed _ -> raise Not_utf8
        | _ -> ()
      ) () str;
    true
  with Not_utf8 -> false

let encode_string str =
  if is_valid_utf8 str
  then string str
  else
    let `Hex h = Hex.of_string str in
    `O [ "hex", string h ]

let decode_string = function
  | `String str               -> Some str
  | `O [ "hex", `String str ] -> Some (Hex.to_string (`Hex str))
  | _                         -> None

let decode_string_exn j =
  match decode_string j with
  | Some s -> s
  | None   -> parse_error j "Ezjsonm.decode_string_exn"

let rec of_sexp = function
  | Sexplib0.Sexp.Atom x -> encode_string x
  | Sexplib0.Sexp.List l -> list of_sexp l

let value_of_sexp = of_sexp

let t_of_sexp s = match value_of_sexp s with
  | `A x -> `A x
  | `O x -> `O x
  | _ -> failwith "Ezjsonm: t_of_sexp encountered a value (fragment) rather than a t"

let rec to_sexp json =
  match decode_string json with
  | Some s -> Sexplib0.Sexp.Atom s
  | None   ->
    match json with
    | `A l -> Sexplib0.Sexp.List (List.map to_sexp l)
    | _    -> parse_error json "Ezjsonm.to_sexp"

let sexp_of_value = to_sexp

let sexp_of_t t = sexp_of_value @@ value t
