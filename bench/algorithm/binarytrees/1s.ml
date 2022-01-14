(* This version has unfair heap allocation 
  that can be verified by callgrind tool
  dune build
  valgrind --tool=callgrind --callgrind-out-file=callgrind.out --  ./_build/default/app.exe 6
  gprof2dot -f callgrind callgrind.out | dot -Tsvg -o perf.svg

  According to https://ocaml.org/manual/intfc.html#ss:c-tuples-and-records and https://ocaml.org/api/Gc.html
  Records are also represented by zero-tagged blocks, only the tree roots in this implementation
  are unboxed and GC footprinted
*)

type node = { left : node option; right : node option }

let rec make d =
  if d = 0 then { left = None; right = None }
  else
    let d = d - 1 in
    { left = Some (make d); right = Some (make d) }

let rec check n =
  1
  + (match n.left with None -> 0 | Some l -> check l)
  + match n.right with None -> 0 | Some r -> check r

let min_depth = 4

let max_depth =
  let n = try int_of_string (Array.get Sys.argv 1) with _ -> 10 in
  max (min_depth + 2) n

let stretch_depth = max_depth + 1

let () =
  let c = stretch_depth |> make |> check in
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c

let long_lived_tree = make max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
    for _ = 1 to niter do
      c := !c + (d |> make |> check)
    done;
    Printf.printf "%i\t trees of depth %i\t check: %i\n" niter d !c
  done

let () =
  flush stdout;
  loop_depths min_depth;
  Printf.printf "long lived tree of depth %i\t check: %i\n" max_depth
    (long_lived_tree |> check)
