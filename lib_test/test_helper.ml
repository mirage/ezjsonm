let json_t: Ezjsonm.t Alcotest.testable =
  (module struct
    type t = Ezjsonm.t
    let equal = (=)
    let pp ppf x = Format.pp_print_string ppf (Ezjsonm.to_string x)
  end)

let json_v: Ezjsonm.value Alcotest.testable =
  (module struct
    type t = Ezjsonm.value
    let equal = (=)
    let pp ppf x = Format.pp_print_string ppf Ezjsonm.(value_to_string x)
  end)

let random_int i =
  if i <= 1 then 0
  else Random.int i

let random_string len =
  let s = Bytes.create (random_int len) in
  for i = 0 to Bytes.length s - 1 do
    Bytes.set s i @@ Char.chr (random_int 127)
  done;
  Bytes.to_string s

let random_list len gen =
  Array.to_list (Array.init len gen)

let random_bool () =
  Random.int 2 = 0
