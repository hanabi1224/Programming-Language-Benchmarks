(* The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * contributed by Troestler Christophe
 * modified by Mauricio Fernandez
 * optimized by Fabrice Le Fessant
 * modified by Roman Kashitsyn: use Bytes instead of String
 *)

 let tab = Array.make 256 0

 let _ =
   tab.(Char.code 'A') <- 0;
   tab.(Char.code 'a') <- 0;
   tab.(Char.code 'T') <- 1;
   tab.(Char.code 't') <- 1;
   tab.(Char.code 'C') <- 2;
   tab.(Char.code 'c') <- 2;
   tab.(Char.code 'g') <- 3;
   tab.(Char.code 'G') <- 3
 
 let uppercase line =
   let len = Bytes.length line in
   for i = 0 to len - 1 do
     let c = Bytes.get line i in
     Bytes.set line i (Char.unsafe_chr tab.(Char.code c))
   done
 
 let dna =
   let n_args = Array.length Sys.argv in
   let file_name =
     match n_args > 1 with true -> Array.get Sys.argv 1 | false -> "25000_in"
   in
   let file_channel = open_in file_name in
   let is_not_three s = String.length s < 6 || String.sub s 0 6 <> ">THREE" in
   while is_not_three (input_line file_channel) do
     ()
   done;
   let buf = Buffer.create 130_000_000 in
   (* Skip possible comment *)
   (try
      while true do
        let line = Bytes.unsafe_of_string (input_line file_channel) in
        if Bytes.get line 0 <> ';' then (
          uppercase line;
          Buffer.add_bytes buf line;
          raise Exit)
      done
    with _ -> ());
   (* Read the DNA sequence *)
   (try
      while true do
        let line = Bytes.unsafe_of_string (input_line file_channel) in
        if Bytes.get line 0 = '>' then raise End_of_file;
        uppercase line;
        Buffer.add_bytes buf line
      done
    with End_of_file -> ());
   Buffer.contents buf
 
 module K15 = struct
   type t = int
 
   let equal k1 k2 = k1 = k2
 
   let hash n = n
 end
 
 module K16 = struct
   type t = int * int
 
   let equal (a1, a2) (b1, b2) = a1 = b1 && a2 = b2
 
   let hash (a1, _) = a1
 end
 
 type entry = { mutable count : int }
 
 let threshold15 =
   match Sys.word_size with 32 -> 15 | 64 -> 31 | _ -> assert false
 
 let threshold16 = threshold15 + 1
 
 let c = 0x40000 - 1
 
 module H15 = Hashtbl.Make (K15)
 module H16 = Hashtbl.Make (K16)
 
 let h15 = H15.create c
 
 let h16 = H16.create c
 
 let rec pack_word n k h =
   let b = Char.code dna.[n] in
   let h = (h * 4) + b in
   if k > 1 then pack_word (n + 1) (k - 1) h else h
 
 let pack15 k n = pack_word n k 0
 
 let pack16 k n =
   let h1 = pack_word n threshold15 0 in
   let h2 = pack_word (n + threshold15) (k - threshold15) 0 in
   (h1, h2)
 
 let rec pack_word_in dna n k h =
   let b = dna.[n] in
   let b = tab.(Char.code b) in
   let h = (h * 4) + b in
   if k > 1 then pack_word_in dna (n + 1) (k - 1) h else h
 
 let pack_key15 seq =
   let k = String.length seq in
   pack_word_in seq 0 k 0
 
 let pack_key16 seq =
   let k = String.length seq in
   let h1 = pack_word_in seq 0 threshold15 0 in
   let h2 = pack_word_in seq threshold15 (k - threshold15) 0 in
   (h1, h2)
 
 let char = [| 'A'; 'T'; 'C'; 'G' |]
 
 let rec unpack h s pos k =
   let pos = pos - 1 in
   Bytes.set s pos char.(h land 3);
   if k > 1 then unpack (h lsr 2) s pos (k - 1)
 
 let unpack15 k h1 =
   let s = Bytes.create k in
   unpack h1 s k k;
   Bytes.unsafe_to_string s
 
 let unpack16 k (h1, h2) =
   let s = Bytes.create k in
   unpack h1 s threshold15 threshold15;
   unpack h2 s k (k - threshold15);
   Bytes.unsafe_to_string s
 
 let count15 k =
   for i = 0 to String.length dna - k - 1 do
     let packed = pack15 k i in
     try
       let key = H15.find h15 packed in
       key.count <- key.count + 1
     with Not_found -> H15.add h15 packed { count = 1 }
   done
 
 let count16 k =
   for i = 0 to String.length dna - k - 1 do
     let packed = pack16 k i in
     try
       let key = H16.find h16 packed in
       key.count <- key.count + 1
     with Not_found -> H16.add h16 packed { count = 1 }
   done
 
 let count k = if k < threshold16 then count15 k else count16 k
 
 let compare_freq ((k1 : string), (f1 : float)) (k2, f2) =
   if f1 > f2 then -1 else if f1 < f2 then 1 else String.compare k1 k2
 
 let write_frequencies15 k =
   count15 k;
   let tot = float (H15.fold (fun _ n t -> n.count + t) h15 0) in
   let frq =
     H15.fold
       (fun h n l -> (unpack15 k h, 100. *. float n.count /. tot) :: l)
       h15 []
   in
   let frq = List.sort compare_freq frq in
   String.concat "" (List.map (fun (k, f) -> Printf.sprintf "%s %.3f\n" k f) frq)
 
 let write_frequencies16 k =
   count16 k;
   let tot = float (H16.fold (fun _ n t -> n.count + t) h16 0) in
   let frq =
     H16.fold
       (fun h n l -> (unpack16 k h, 100. *. float n.count /. tot) :: l)
       h16 []
   in
   let frq = List.sort compare_freq frq in
   String.concat "" (List.map (fun (k, f) -> Printf.sprintf "%s %.3f\n" k f) frq)
 
 let write_count15 k seq =
   count15 k;
   Printf.sprintf "%d\t%s"
     (try (H15.find h15 (pack_key15 seq)).count with Not_found -> 0)
     seq
 
 let write_count16 k seq =
   count16 k;
   Printf.sprintf "%d\t%s"
     (try (H16.find h16 (pack_key16 seq)).count with Not_found -> 0)
     seq
 
 let write_frequencies k =
   if k < threshold16 then write_frequencies15 k else write_frequencies16 k
 
 let write_count seq =
   let k = String.length seq in
   if k < threshold16 then write_count15 k seq else write_count16 k seq
 
 type t = Size of int | Dna of string
 
 let invoke (f : t -> string) x : unit -> string =
   let input, output = Unix.pipe () in
   match Unix.fork () with
   | -1 ->
       Unix.close input;
       Unix.close output;
       let v = f x in
       fun () -> v
   | 0 ->
       Unix.close input;
       let output = Unix.out_channel_of_descr output in
       Marshal.to_channel output (f x) [];
       close_out output;
       exit 0
   | pid ->
       Unix.close output;
       let input = Unix.in_channel_of_descr input in
       fun () ->
         let v = Marshal.from_channel input in
         ignore (Unix.waitpid [] pid);
         close_in input;
         v
 
 let parallelize f l =
   let list = List.map (invoke f) (List.rev l) in
   List.iter (fun g -> print_endline (g ())) (List.rev list)
 
 let () =
   parallelize
     (fun i ->
       match i with Size i -> write_frequencies i | Dna k -> write_count k)
     [
       Size 1;
       Size 2;
       Dna "GGT";
       Dna "GGTA";
       Dna "GGTATT";
       Dna "GGTATTTTAATT";
       Dna "GGTATTTTAATTTATAGT";
     ]
 