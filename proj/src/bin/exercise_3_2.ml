module List = Proj_src.Linked_list

let lst = 
  let lst' = List.create_list ()
  List.insert_list lst' 1;
  List.insert_list lst' 2;
  List.insert_list lst' 3;
  lst'