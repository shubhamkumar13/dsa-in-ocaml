module List = Proj_src.Linked_list

let _ = List.delete_list [ 1; 2; 3 ] 3 |> List.iter (Printf.printf "%d\n")
