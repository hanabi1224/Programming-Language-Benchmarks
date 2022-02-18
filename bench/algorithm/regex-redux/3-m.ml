(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * regex-dna program contributed by Christophe TROESTLER
 * updated by Roman Kashitsyn: use Bytes instead of String
 * updated by Gaëtan Dubreil: use the Re library and parallelize processing
 *)

 open Printf

 let variants =
   [
     "agggtaaa|tttaccct";
     "[cgt]gggtaaa|tttaccc[acg]";
     "a[act]ggtaaa|tttacc[agt]t";
     "ag[act]gtaaa|tttac[agt]ct";
     "agg[act]taaa|ttta[agt]cct";
     "aggg[acg]aaa|ttt[cgt]ccct";
     "agggt[cgt]aa|tt[acg]accct";
     "agggta[cgt]a|t[acg]taccct";
     "agggtaa[cgt]|[acg]ttaccct";
   ]
 
 let subst =
   [
     ("tHa[Nt]", "<4>");
     ("aND|caN|Ha[DS]|WaS", "<3>");
     ("a[NSt]|BY", "<2>");
     ("<[^>]*>", "|");
     ("\\|[^|][^|]*\\|", "-");
   ]
 
 (* Read all of a redirected FASTA format file from file. *)
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
 let dna = Re.replace_string (Re.Pcre.regexp ">.*\n|\n") ~by:"" file_data
 
 let code_length = String.length dna
 
 (* Count matches of [re]. *)
 let count re s =
   let re = Re.Pcre.regexp re in
   let i = ref 0 in
   let n = ref 0 in
   try
     while true do
       let grps = Re.exec ~pos:!i re s in
       i := Re.Group.stop grps 0;
       incr n
     done;
     assert false
   with Not_found -> !n
 
 let () =
   if Unix.fork () = 0 then
     List.iter (fun re -> printf "%s %i\n" re (count re dna)) variants
   else
     let b = ref dna in
     List.iter
       (fun (re, s) -> b := Re.replace_string (Re.Pcre.regexp re) ~by:s !b)
       subst;
 
     ignore (Unix.wait ());
     printf "\n%i\n%i\n%i\n" file_length code_length (String.length !b)
 