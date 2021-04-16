type node = { value : int; mutable next : node option }

type t = { mutable head : node option }

let unwrap : 'a option -> 'a = function
  | None -> failwith "Can't unwrap"
  | Some x -> x

let create_node : int -> node = fun x -> { value = x; next = None }

let create_list : unit -> t = fun () -> { head = None }

let insert_list : t -> int -> unit =
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

let delete_list = fun lst x ->
  let p = {head=lst.head} in
  let rec delete_list' = fun lst ->
    match lst.head with
    | None -> failwith "Empty list"
    | Some node ->
      if (unwrap node.next).value = x then
        node.next <- (unwrap node.next).next
      else begin
        p.head <- node.next;
        delete_list' p
      end
    in
  delete_list' p
        

let fmt_list : t -> string =
 fun lst ->
  let rec fmt_list' lst acc =
    match lst.head with
    | None -> List.rev acc
    | Some node -> fmt_list' { head = node.next } (node.value :: acc)
  in
  List.fold_left
    (fun acc x -> acc ^ Printf.sprintf " -> %d" x)
    "" (fmt_list' lst [])
