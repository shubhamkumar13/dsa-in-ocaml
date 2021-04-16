type node = { value : int; mutable next : node option }

type t = { mutable top : node option }

let create_node = fun element -> {value=element; next=None}

let create_stack = fun () -> {top=None}

let push = fun stack element ->
  let node = create_node element in
  node.next <- stack.top;
  stack.top <- Some node  

let pop = fun stack ->
  match stack.top with
  | None -> failwith "The stack is empty"
  | Some node -> let out = node.value in
    stack.top <- node.next;
    out

let fmt : t -> string =
 fun stack ->
  let rec fmt' stack acc =
    match stack.top with
    | None -> List.rev acc
    | Some node -> fmt' { top = node.next } (node.value :: acc)
  in
  List.fold_left
    (fun acc x -> acc ^ Printf.sprintf " -> %d" x)
    "" (fmt' stack [])