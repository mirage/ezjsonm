let () = match Ezjsonm.from_string_result Contracts.v with
  | Ok _v -> Format.printf "[OK] huge list loaded.\n%!"
  | Error err -> Format.eprintf "[ER] %s.\n%!" (Ezjsonm.read_error_description err)

let () = match Ezjsonm.from_string_result Script.v with
  | Ok _v -> Format.printf "[OK] huge object loaded.\n%!"
  | Error err -> Format.eprintf "[ER] %s.\n%!" (Ezjsonm.read_error_description err)
