(* The Computer Language Benchmarks Game
   https://salsa.debian.org/benchmarksgame-team/benchmarksgame/

   contributed by Ethan Burns
*)


(** Flip the front [n] pancakes of [a]. *)
let flip n (a : int array) =
  for i = 0 to n / 2 do
    let t = a.(i) in
    let k = n - i in
      a.(i) <- a.(k);
      a.(k) <- t;
  done

(** Count the number of flips so that pancake 0 is at index 0. *)
let rec count c ary =
  let z = ary.(0) in
    if z <> 0 then begin
      flip z ary;
      count (c + 1) ary
    end else
      c

(** Rotate the first [n] pancakes of [a]. *)
let rotate n (a : int array) =
  let t = a.(0) in
  let m = n - 1 in
    for i = 1 to m do
      a.(i - 1) <- a.(i);
    done;
    a.(m) <- t

(** Call [f] on each permutation of [n] numbers in order. *)
let iter_perms n f =
  let rec do_iter num perm copy f ht =
    if ht = 1 then begin
      for i = 0 to n - 1 do copy.(i) <- perm.(i) done;
      f !num copy;
      incr num;
    end else
      for _ = 1 to ht do
	do_iter num perm copy f (ht - 1);
	rotate ht perm;
      done
  in
  let perm = Array.init n (fun i -> i) in
  let copy = Array.make n 0 in
  let num = ref 0 in
    do_iter num perm copy f n

let _ =
  let n = int_of_string Sys.argv.(1) in
  let csum = ref 0 and m = ref 0 in
    iter_perms n (fun num a ->
		    let c = count 0 a in
		      (* csum update from Otto Bommer's Scala ver. *)
		      csum := !csum + c * (1 - (num land 1) lsl 1);
		      if c > !m then m := c;);
    Printf.printf "%d\nPfannkuchen(%d) = %d\n" !csum n !m