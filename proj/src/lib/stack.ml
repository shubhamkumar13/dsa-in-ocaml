type 'a t = {
  top : int option
  size : int
  arr : 'a option array 
}

let unwrap_fail : 'a option -> 'a = function
  | None -> failwith "What you are trying to unwrap is a None type"
  | Some(x) -> x

let is_empty : 'a t -> bool = fun _ -> false

let is_full : 'a t -> bool = fun _ -> false

let create : unit -> 'a t = fun () ->
  let top = None in
  let size = 100 in
  let arr = Array.make size None in
  {top; size; arr}

let push : 'a t -> 'a -> unit = fun stack x ->
  let _ = if is_full arr then expand arr else () in
  match stack.top with
  | None -> begin
    stack.top <- Some(0);
    stack.arr.(unwrap_fail stack.top) <- Some(x);
  end
  | Some(l) -> begin
    stack.top <- Some(unwrap_fail top + 1);
    stack.arr.(unwrap_fail stack.top) <- Some(x);
  end


let pop : 'a t -> 'a = ()