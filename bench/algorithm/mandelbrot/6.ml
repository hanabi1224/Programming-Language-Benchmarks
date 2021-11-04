(*
 * The Computer Language Benchmarks Game
 * https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
 *
 * Contributed by Paolo Ribeca
 *
 * (Very loosely based on previous version Ocaml #3,
 *  which had been contributed by
 *   Christophe TROESTLER
 *  and enhanced by
 *   Christian Szegedy and Yaron Minsky)
 *)

 let niter = 50

 let limit = 4.
 
 let norm_n n = (n + 7) / 8 * 8
 
 let () =
   let w = Array.get Sys.argv 1 |> int_of_string |> norm_n in
   let h = w in
   let fw = float w /. 2. and fh = float h /. 2. in
   Printf.printf "P4\n%i %i\n" w h;
   let red_h = h - 1 and red_w = w - 1 and byte = ref 0 in
   let pixels = Buffer.create (w * h / 8) in
   for y = 0 to red_h do
     let ci = (float y /. fh) -. 1. in
     for x = 0 to red_w do
       let cr = (float x /. fw) -. 1.5
       and zr = ref 0.
       and zi = ref 0.
       and trmti = ref 0.
       and n = ref 0 in
       (try
          while true do
            zi := (2. *. !zr *. !zi) +. ci;
            zr := !trmti +. cr;
            let tr = !zr *. !zr and ti = !zi *. !zi in
            if tr +. ti > limit then (
              byte := !byte lsl 1;
              raise Exit)
            else if
              incr n;
              !n = niter
            then (
              byte := (!byte lsl 1) lor 0x01;
              raise Exit)
            else trmti := tr -. ti
          done
        with Exit -> ());
       if x mod 8 == 7 then Buffer.add_uint8 pixels !byte
     done
   done;
   let hash = pixels |> Buffer.to_bytes |> Digest.bytes |> Digest.to_hex in
   Printf.printf "%s\n" hash
 