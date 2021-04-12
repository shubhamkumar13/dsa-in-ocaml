include List

let search_list : 'a t -> 'a -> bool =
 fun lst x ->
  match List.find_opt (fun a -> a = x) lst with None -> false | Some _ -> true

let insert_list : 'a t -> 'a -> 'a t =
 fun lst x -> match lst with [] -> [ x ] | lst -> rev @@ x :: rev lst

let delete_list : 'a t -> 'a -> 'a t =
 fun lst x ->
  let rec aux acc lst =
    match lst with
    | [] -> failwith "This list is empty"
    | hd :: tl -> if hd = x then append (rev acc) tl else aux (hd :: acc) tl
  in
  aux [] lst
