class node left' right' =
  object
    val left : node option = left'

    val right : node option = right'

    method check =
      1
      + (match left with None -> 0 | Some l -> l#check)
      + match right with None -> 0 | Some r -> r#check
  end

let rec make d =
  if d = 0 then new node None None
  else
    let d = d - 1 in
    new node (Some (make d)) (Some (make d))

let min_depth = 4

let max_depth =
  let n = try int_of_string (Array.get Sys.argv 1) with _ -> 10 in
  max (min_depth + 2) n

let stretch_depth = max_depth + 1

let () =
  let c = (make stretch_depth)#check in
  Printf.printf "stretch tree of depth %i\t check: %i\n" stretch_depth c

let long_lived_tree = make max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
    for _ = 1 to niter do
      c := !c + (make d)#check
    done;
    Printf.printf "%i\t trees of depth %i\t check: %i\n" niter d !c
  done

let () =
  flush stdout;
  loop_depths min_depth;
  Printf.printf "long lived tree of depth %i\t check: %i\n" max_depth
    long_lived_tree#check
