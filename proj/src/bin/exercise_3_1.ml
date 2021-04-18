(* parentheses balancing *)

let test_cases = ["((())())()"; ")()("; "())"]

module Stack = Proj_src.Stack

let signal paren stack =
  match paren with
  | '(' ->
    (* push paren to stack *)
    Stack.push stack paren;
    Ok ()
  | ')' ->
    (* pop paren from stack *)
    (* check for empty stack, if stack is empty you can't pop also this might be not properly nested  *)
      if Stack.is_empty stack then
        Error "Stack is empty"
      else
        Stack.pop stack |> fun _ -> Ok ()
  | _ -> failwith "This is not a parenthesis"

let f str =
  let stack = Stack.create_stack () in
  let rec f' str stack i = 
    if i < (String.length str) then
      match signal (String.get str i) stack with
      | Error _ -> Error "stop"
      | Ok _ -> f' str stack (i + 1)
    else
      Ok () in
  match f' str stack 0 with
  | Error _ -> false
  | _ -> if Stack.is_empty stack then true else false

let _ = List.iter (fun s -> if f s then Printf.printf "true\n" else Printf.printf "false\n") test_cases

(* to get the full credit just modify the function [signal paren stack] to [signal paren stack i] *)
(* this i can be passed on in the error context and can easily be viewed *)