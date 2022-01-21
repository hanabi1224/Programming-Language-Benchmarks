let merge l r = l + r
let cal_hash i = i

class node value' left' right' =
  object
    val value : int option = value'
    val left : node option = left'
    val right : node option = right'
    val mutable hash : int option = None
    method get_hash = match hash with Some h -> h | _ -> -1

    method check =
      match hash with
      | Some _ -> (
          match value with
          | Some _ -> true
          | _ ->
              let l = match left with Some n -> n#check | _ -> false in
              let r = match right with Some n -> n#check | _ -> false in
              l && r)
      | _ -> false

    method cal_merkle_hash =
      match hash with
      | Some _ -> ()
      | _ -> (
          match value with
          | Some v ->
              let h = cal_hash v in
              hash <- Some h;
              ()
          | _ ->
              let h_left =
                match left with
                | Some n ->
                    n#cal_merkle_hash;
                    n#get_hash
                | _ -> 0
              in
              let h_right =
                match right with
                | Some n ->
                    n#cal_merkle_hash;
                    n#get_hash
                | _ -> 0
              in
              let h = merge h_left h_right in
              hash <- Some h;
              ())
  end

let rec make d =
  if d = 0 then new node (Some 1) None None
  else
    let d = d - 1 in
    new node None (Some (make d)) (Some (make d))

let min_depth = 4

let max_depth =
  let n = try int_of_string (Array.get Sys.argv 1) with _ -> 10 in
  max (min_depth + 2) n

let stretch_depth = max_depth + 1

let () =
  let stretch_tree = make stretch_depth in
  stretch_tree#cal_merkle_hash;
  Printf.printf "stretch tree of depth %i\t root hash: %i check: %b\n"
    stretch_depth stretch_tree#get_hash stretch_tree#check

let long_lived_tree = make max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
    for _ = 1 to niter do
      let t = make d in
      t#cal_merkle_hash;
      c := !c + t#get_hash
    done;
    Printf.printf "%i\t trees of depth %i\t root hash sum: %i\n" niter d !c
  done

let () =
  loop_depths min_depth;
  long_lived_tree#cal_merkle_hash;
  Printf.printf "long lived tree of depth %i\t root hash: %i check: %b\n"
    max_depth long_lived_tree#get_hash long_lived_tree#check

(* let () = Printf.printf "%d\n" Sys.int_size *)
