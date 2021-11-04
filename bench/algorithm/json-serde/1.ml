module JsonT = Yojson.Safe

let print_hash json =
  let s = json |> JsonT.to_string ~std:true in
  let hash = s |> Digest.string |> Digest.to_hex in
  (* Printf.printf "%s\n" s; *)
  Printf.printf "%s\n" hash

let _ =
  let n_args = Array.length Sys.argv in
  let file_name =
    match n_args > 1 with true -> Array.get Sys.argv 1 | false -> "sample"
  in
  let n =
    match n_args > 2 with
    | true -> Array.get Sys.argv 2 |> int_of_string
    | false -> 3
  in
  let file_name_with_ext = Printf.sprintf "%s.json" file_name in
  let file_channel = open_in file_name_with_ext in
  let file_channel_length =
    file_channel |> LargeFile.in_channel_length |> Int64.to_int
  in
  let json_str = really_input_string file_channel file_channel_length in
  let json = JsonT.from_string json_str in
  print_hash json;
  let json_array = ref [] in
  for _i = 1 to n do
    let json = json_str |> JsonT.from_string in
    json_array := json :: !json_array;
    print_hash (`List !json_array)
  done
