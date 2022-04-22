type 'a linked_list_node = {
  mutable prev : 'a linked_list_node option;
  mutable next : 'a linked_list_node option;
  mutable data : 'a;
}

type 'a linked_list = {
  mutable head : 'a linked_list_node option;
  mutable tail : 'a linked_list_node option;
  mutable len : int;
}

let _add_node list node =
  (match list.head with
  | None ->
      list.head <- Some node;
      node.prev <- None
  | _ -> (
      match list.tail with
      | Some tail ->
          node.prev <- list.tail;
          tail.next <- Some node
      | _ -> ()));
  node.next <- None;
  list.tail <- Some node

let _remove list node =
  (match list.head with
  | Some head -> if head == node then list.head <- head.next
  | _ -> ());
  (match list.tail with
  | Some tail -> if tail == node then list.tail <- tail.prev
  | _ -> ());
  (match node.prev with Some prev -> prev.next <- node.next | _ -> ());
  match node.next with Some next -> next.prev <- node.prev | _ -> ()

let add list data =
  let node = { prev = None; next = None; data } in
  _add_node list node;
  list.len <- list.len + 1;
  node

let move_to_end list node =
  _remove list node;
  _add_node list node

type rng = { mutable seed : int }

let next rng =
  rng.seed <- ((1103515245 * rng.seed) + 12345) mod 2147483648;
  rng.seed

(* type ('a, 'b) pair = 'a * 'b has simliar performance *)
type ('a, 'b) pair = { k : 'a; v : 'b }

type ('a, 'b) lru = {
  mutable size : int;
  keys : ('a, ('a, 'b) pair linked_list_node) Hashtbl.t;
  entries : ('a, 'b) pair linked_list;
}

let new_lru size =
  {
    size;
    keys = Hashtbl.create size;
    entries = { len = 0; head = None; tail = None };
  }

let get_opt lru key =
  match Hashtbl.find_opt lru.keys key with
  | Some node ->
      move_to_end lru.entries node;
      Some node.data.v
  | _ -> None

let put lru k v =
  match Hashtbl.find_opt lru.keys k with
  | Some node ->
      node.data <- { k; v };
      move_to_end lru.entries node
  | _ ->
      if lru.entries.len == lru.size then
       (match lru.entries.head with
       | Some head -> (
         Hashtbl.remove lru.keys head.data.k;
         head.data <- { k; v };
         Hashtbl.add lru.keys k head;
         move_to_end lru.entries head
       )
       | _ -> ())
      else (let new_node = add lru.entries { k; v } in
       Hashtbl.add lru.keys k new_node)
      
let () =
  let size = try int_of_string (Array.get Sys.argv 1) with _ -> 100 in
  let n = try int_of_string (Array.get Sys.argv 2) with _ -> 1000 in
  let modular = size * 10 in
  let rng0 = { seed = 0 } in
  let rng1 = { seed = 1 } in
  let cache = new_lru size in
  let hit = ref 0 in
  let missed = ref 0 in
  for _ = 1 to n do
    let n0 = next rng0 mod modular in
    put cache n0 n0;
    let n1 = next rng1 mod modular in
    match get_opt cache n1 with
    | None -> missed := !missed + 1
    | _ -> hit := !hit + 1
  done;
  Printf.printf "%d\n%d\n" !hit !missed
