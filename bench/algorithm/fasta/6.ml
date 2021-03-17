(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Troestler Christophe
 * modified by Mauricio Fernandez
 * modified by Fabrice Le Fessant
 * modified by Roman Kashitsyn: use Bytes instead of String
 *)

(* Random number generator *)


let im = 139968
let ia = 3877
let ic = 29573
let width = 60
let im_inv_f = 1. /. float im

let rand_char last ps line j chars =
  let n = (last * ia + ic) mod im in
  let p = float n *. im_inv_f in
  let len = Array.length ps in
  let i = len/2 in
  let i =
  if p >= ps.(i) then
    let i = ref (i+1) in
    while p >= ps.(!i) do incr i done;
    !i
  else
    let i = ref 0 in
    while p >= ps.(!i) do incr i done;
    !i
  in
  Bytes.set line j chars.(i);
  n

let make_random_fasta last id desc table n =
  print_char '>'; print_string id;
  print_char ' '; print_string desc;
  print_char '\n';
  let len_table = Array.length table in
  let ps = Array.make len_table 0. in
  let chars = Array.make len_table '\000' in
  let p = ref 0.0 in
  for i = 0 to len_table -1 do
    let (c, px) = table.(i) in
    chars.(i) <- c;
    p := !p +. px;
    ps.(i) <- !p;
  done;
  let nlines = (n + width - 1) / width in
  let nchars = n + nlines in
  let line = Bytes.create nchars in
  let last_n = ref last in
  let pos = ref 0 in
  for i = 1 to nlines-1 do
    let current_pos = !pos in
    let final_pos = current_pos + width-1 in
    for j = current_pos to final_pos do
      last_n := rand_char !last_n ps line j chars;
    done;
    pos := final_pos + 2;
    Bytes.set line (final_pos + 1) '\n';
  done;
  for j = !pos to nchars - 2 do
    last_n := rand_char !last_n ps line j chars;
  done;
  Bytes.set line (nchars - 1) '\n';
  output_bytes stdout line;
  !last_n

(* [write s i0 l w] outputs [w] chars of [s.[0 .. l]], followed by a
   newline, starting with [s.[i0]] and considering the substring [s.[0
   .. l]] as a "circle".
   One assumes [0 <= i0 <= l <= Bytes.length s].
   @return [i0] needed for subsequent writes.  *)
let rec write s i0 l w =
  let len = l - i0 in
  if w <= len then (output stdout s i0 w; print_char '\n'; i0 + w)
  else (output stdout s i0 len; write s 0 l (w - len))

let make_repeat_fasta id desc src n =
  print_char '>';
  print_string id;
  print_char ' ';
  print_string desc;
  print_char '\n';
  let l = Bytes.length src
  and i0 = ref 0 in
  for i = 1 to n / width do
    i0 := write src !i0 l width;
  done;
  let w = n mod width in
  if w > 0 then ignore(write src !i0 l w)


let alu = "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

let iub = [| ('a', 0.27);  ('c', 0.12);  ('g', 0.12);  ('t', 0.27);
	     ('B', 0.02);  ('D', 0.02);  ('H', 0.02);  ('K', 0.02);
	     ('M', 0.02);  ('N', 0.02);  ('R', 0.02);  ('S', 0.02);
	     ('V', 0.02);  ('W', 0.02);  ('Y', 0.02);  |]

let homosapiens = [| ('a', 0.3029549426680);    ('c', 0.1979883004921);
		     ('g', 0.1975473066391);    ('t', 0.3015094502008);  |]

let () =
  let n = try int_of_string(Array.get Sys.argv 1) with _ -> 1000 in
  make_repeat_fasta "ONE" "Homo sapiens alu" (Bytes.of_string alu) (n*2);
  let last = 42 in
  let last = make_random_fasta last "TWO" "IUB ambiguity codes" iub (n*3) in
  let _last =
    make_random_fasta last "THREE" "Homo sapiens frequency" homosapiens (n*5)
  in
  ()