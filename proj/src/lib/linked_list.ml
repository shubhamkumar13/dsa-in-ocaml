type 'a node = { value : 'a; mutable next : 'a node option }

type 'a t = { mutable head : 'a node option }

let unwrap : 'a option -> 'a = function
  | None -> failwith "Can't unwrap"
  | Some x -> x

let create_node : 'a -> 'a node = fun x -> { value = x; next = None }

let create_list : unit -> 'a t = fun () -> { head = None }

let insert_list : 'a t -> 'a -> unit =
 fun lst x ->
  let node = create_node x in

  (* now we have a node which has a value and it points to nothing
     to add it to the list we have to replace the head with the new node
     at the same time attach the previous head to the next record of the new node *)
  let old_node = lst.head in
  (match old_node with
  | None -> node.next <- None
  | Some x -> node.next <- Some x);
  lst.head <- Some node

let delete_list lst x =
  let p = { head = lst.head } in
  let rec delete_list' lst =
    match lst.head with
    | None -> failwith "Empty list"
    | Some node ->
        if (unwrap node.next).value = x then
          node.next <- (unwrap node.next).next
        else (
          p.head <- node.next;
          delete_list' p)
  in
  delete_list' p

(* let fmt_list : 'a t -> string =
 fun lst ->
  let rec fmt_list' lst acc =
    match lst.head with
    | None -> List.rev acc
    | Some node -> fmt_list' { head = node.next } (node.value :: acc)
  in
  List.fold_left
    (fun acc x -> acc ^ Printf.sprintf " -> %d" x)
    "" (fmt_list' lst []) *)
