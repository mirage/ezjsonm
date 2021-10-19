open Test_helper

let stream0 =
  random_list 42 (fun i -> Ezjsonm.(string @@ random_string i))
   
let test_stream jsons () =
  let jsons = List.map Ezjsonm.wrap jsons in
  let jsons, last = match jsons with
    | []   -> assert false
    | h::t -> t, h
  in
  let string j = Ezjsonm.to_string j in
  let strings = List.map (fun j -> string j ^ ",") jsons in
  let strings = "[" :: strings @ [string last; "]"] in
  let jsons = jsons @ [last] in
  Lwt_main.run begin
    let open Lwt.Infix in
    let stream = Lwt_stream.of_list strings in
    let stream = Ezjsonm_lwt.from_stream stream in
    Lwt_stream.to_list stream >|= fun json' ->
    Alcotest.(check @@ list json_v) "stream" jsons json'
  end

let () =
  Alcotest.run "ezjsonm" [
    "stream", [
      "stream0", `Quick, test_stream stream0;
    ];
  ]
