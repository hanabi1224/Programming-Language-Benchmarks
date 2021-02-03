let get_name = function argv ->
  if Array.length argv > 0 then
    Array.get argv 1
  else
    "";;

let () =
  Printf.printf "Hello world %s!\n" (get_name Sys.argv)
