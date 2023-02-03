open Domainslib

let rec generate_inner chan n cancel =
  if !cancel == true then ()
  else (
    Chan.send chan n;
    generate_inner chan (n + 1) cancel;
    ())

let generate chan cancel = generate_inner chan 2 cancel

let rec filter chan_in chan_out prime cancel =
  if !cancel == true then ()
  else
    let n = Chan.recv chan_in in
    if n mod prime != 0 then Chan.send chan_out n else ();
    filter chan_in chan_out prime cancel;
    ()

let main pool n =
  let cancel = ref false in
  let tasks = ref [] in
  let chan0 = Chan.make_bounded 1 in
  let t1 = Task.async pool (fun _ -> generate chan0 cancel) in
  tasks := !tasks @ [ t1 ];
  let chans = ref (Array.of_list [ chan0 ]) in
  for i = 1 to n do
    let chan_in = !chans.(i - 1) in
    let chan_out = Chan.make_bounded 1 in
    chans := Array.append !chans (Array.of_list [ chan_out ]);
    let prime = Chan.recv chan_in in
    Printf.printf "%d\n" prime;
    let t = Task.async pool (fun _ -> filter chan_in chan_out prime cancel) in
    tasks := !tasks @ [ t ]
  done;
  cancel := true;
  tasks

let () =
  let n = try int_of_string (Array.get Sys.argv 1) with _ -> 100 in
  let pool = Task.setup_pool ~num_domains:n () in
  let tasks = Task.run pool (fun _ -> main pool n) in
  let array = Array.of_list !tasks in
  for i = 0 to Array.length array - 1 do
    Task.await pool array.(i)
  done;
  Task.teardown_pool pool
