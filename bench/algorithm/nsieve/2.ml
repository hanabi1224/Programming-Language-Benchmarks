module Bits = Res.Bits

let nsieve n =
  let count = ref 0 in
  let flags = Bits.make n true in
  for i = 2 to n - 1 do
    if Bits.get flags i then (
      let j = ref (i lsl 1) in
      while !j < n do
        Bits.set flags !j false ;
        j := !j + i
      done ;
      count := !count + 1 )
    else ()
  done ;
  Printf.printf "Primes up to %8d %8d\n" n !count

let get_n =
  if Array.length Sys.argv > 1 then
    try int_of_string Sys.argv.(1) with _ -> 10
  else 10

let () =
  let n = get_n in
  for i = 0 to 2 do
    10000 lsl (n - i) |> nsieve
  done
