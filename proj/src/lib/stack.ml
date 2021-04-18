type 'a node = { value : 'a; mutable next : 'a node option }

type 'a t = { mutable top : 'a node option }

let create_node element = { value = element; next = None }

let create_stack () = { top = None }

let is_empty : 'a t -> bool = fun stack ->
  match stack.top with
  | None -> true
  | Some(_) -> false

let push stack element =
  let node = create_node element in
  node.next <- stack.top;
  stack.top <- Some node

let pop stack =
  match stack.top with
  | None -> failwith "The stack is empty"
  | Some node ->
      let out = node.value in
      stack.top <- node.next;
      out

(* let fmt : 'a -> string =
 fun stack ->
  let rec fmt' stack acc =
    match stack.top with
    | None -> List.rev acc
    | Some node -> fmt' { top = node.next } (node.value :: acc)
  in
  List.fold_left
    (fun acc x -> acc ^ Printf.sprintf " -> %d" x)
    "" (fmt' stack []) *)
