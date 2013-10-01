let print_list print_f xs =
  print_string "[ ";
  List.iter (fun x -> print_f x; print_string "; ") xs;
  print_endline "]"

let print_array print_f xs =
  print_string "< ";
  List.iter (fun x -> print_f x; print_string "; ") xs;
  print_endline ">"

let print_int_list = print_list print_int

(* 1 -- 2 -- 3    4 -- 5 *)
let small_test () =
  let edges = [(1, 2); (2, 3); (4, 5)] in
  let g = GroupGraph.union 5 edges in
  print_int_list (GroupGraph.group_ids g);;
  

small_test ();;
