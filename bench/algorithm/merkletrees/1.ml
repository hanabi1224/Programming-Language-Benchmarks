type node = {
  mutable hash : int option;
  value : int option;
  left : node option;
  right : node option;
}

let rec make d =
  if d = 0 then { hash = None; value = Some 1; left = None; right = None }
  else
    let d = d - 1 in
    { hash = None; value = None; left = Some (make d); right = Some (make d) }

let merge l r = l + r
let hash i = i
let get_hash node = match node.hash with Some h -> h | _ -> -1

let rec check node =
  match node.hash with
  | Some _ -> (
      match node.value with
      | Some _ -> true
      | _ ->
          let l = match node.left with Some n -> check n | _ -> false in
          let r = match node.right with Some n -> check n | _ -> false in
          l && r)
  | _ -> false

let rec cal_merkle_hash node =
  match node.hash with
  | Some _ -> ()
  | _ -> (
      match node.value with
      | Some v ->
          let h = hash v in
          node.hash <- Some h;
          ()
      | _ ->
          let h_left =
            match node.left with
            | Some n ->
                cal_merkle_hash n;
                get_hash n
            | _ -> 0
          in
          let h_right =
            match node.right with
            | Some n ->
                cal_merkle_hash n;
                get_hash n
            | _ -> 0
          in
          let h = merge h_left h_right in
          node.hash <- Some h;
          ())

let min_depth = 4

let max_depth =
  let n = try int_of_string (Array.get Sys.argv 1) with _ -> 10 in
  max (min_depth + 2) n

let stretch_depth = max_depth + 1

let () =
  let stretch_tree = make stretch_depth in
  cal_merkle_hash stretch_tree;
  Printf.printf "stretch tree of depth %i\t root hash: %i check: %b\n"
    stretch_depth (stretch_tree |> get_hash) (stretch_tree |> check)

let long_lived_tree = make max_depth

let loop_depths d =
  for i = 0 to ((max_depth - d) / 2) + 1 - 1 do
    let d = d + (i * 2) in
    let niter = 1 lsl (max_depth - d + min_depth) in
    let c = ref 0 in
    for _ = 1 to niter do
      let t = make d in
      cal_merkle_hash t;
      c := !c + (t |> get_hash)
    done;
    Printf.printf "%i\t trees of depth %i\t root hash sum: %i\n" niter d !c
  done

let () =
  loop_depths min_depth;
  cal_merkle_hash long_lived_tree;
  Printf.printf "long lived tree of depth %i\t root hash: %i check: %b\n"
    max_depth
    (long_lived_tree |> get_hash)
    (check long_lived_tree)

(* let () = Printf.printf "%d\n" Sys.int_size *)