open Grouper

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
  let g = UncompressedGrouper.union 5 edges in
  print_int_list (UncompressedGrouper.group_ids g);
  Printf.printf "%d %d %d\n" 
    (UncompressedGrouper.group_size g 4)
    (UncompressedGrouper.group_size g 1)
    (UncompressedGrouper.group_size g 0);;

small_test ();;
