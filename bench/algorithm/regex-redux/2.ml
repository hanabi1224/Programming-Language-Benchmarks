(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * regex-dna program contributed by Christophe TROESTLER
 * converted from regex-dna program
 *
 * updated by Roman Kashitsyn: use Bytes instead of String
 *)

 open Printf

 let variants =
   [
     "agggtaaa\\|tttaccct";
     "[cgt]gggtaaa\\|tttaccc[acg]";
     "a[act]ggtaaa\\|tttacc[agt]t";
     "ag[act]gtaaa\\|tttac[agt]ct";
     "agg[act]taaa\\|ttta[agt]cct";
     "aggg[acg]aaa\\|ttt[cgt]ccct";
     "agggt[cgt]aa\\|tt[acg]accct";
     "agggta[cgt]a\\|t[acg]taccct";
     "agggtaa[cgt]\\|[acg]ttaccct";
   ]
 
 (* Remove the "\\" which is mandatory in OCaml regex. *)
 let re_bs = Str.regexp_string "\\"
 let to_string = Str.global_replace re_bs ""
 
 let subst =
   [
     ("tHa[Nt]", "<4>");
     ("aND\\|caN\\|Ha[DS]\\|WaS", "<3>");
     ("a[NSt]\\|BY", "<2>");
     ("<[^>]*>", "|");
     ("|[^|][^|]*|", "-");
   ]
 
 (* Read all of a redirected FASTA format file from stdin. *)
 let file_data, file_length =
   let n_args = Array.length Sys.argv in
   let file_name =
     match n_args > 1 with true -> Array.get Sys.argv 1 | false -> "25000_in"
   in
   let file_channel = open_in file_name in
   let b = Buffer.create 0xFFFF in
   let s = Bytes.create 0xFFF in
   let r = ref 1 in
   while !r > 0 do
     r := input file_channel s 0 0xFFF;
     Buffer.add_substring b (Bytes.unsafe_to_string s) 0 !r
   done;
   (Buffer.contents b, Buffer.length b)
 
 (* Remove FASTA sequence descriptions and all linefeed characters.  *)
 let dna = Str.global_replace (Str.regexp "^>.*$\\|\n") "" file_data
 let code_length = String.length dna
 
 (* Count matches of [re]. *)
 let count re s =
   let re = Str.regexp_case_fold re in
   let i = ref 0 in
   let n = ref 0 in
   try
     while true do
       i := 1 + Str.search_forward re s !i;
       incr n
     done;
     assert false
   with Not_found -> !n
 
 let () =
   List.iter (fun re -> printf "%s %i\n" (to_string re) (count re dna)) variants;
   let b = ref dna in
   List.iter (fun (re, s) -> b := Str.global_replace (Str.regexp re) s !b) subst;
   printf "\n%i\n%i\n%i\n" file_length code_length (String.length !b)
 