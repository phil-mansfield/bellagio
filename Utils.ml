let valOrder (x, y) = if x < y then (x, y) else (y, x)
let keyOrder ((k1, v1), (k2, v2)) = if v1 < v2 then (k1, k2) else (k2, k1)

let id x = x

let tabulate_list (f : int -> 'a) (len : int) : 'a list = 
  let rec tabulate' xs i =
    match i with 
      0 -> (f 0) :: xs
    | _ -> tabulate' ((f i) ::  xs) (i - 1) in
  tabulate' [] (len - 1)

let enumerate_list (xs : 'a list) : (int * 'a) list = 
  List.combine (tabulate_list id (List.length xs)) xs

let print_list (f : 'a -> unit) (xs : 'a list) : unit = 
  let rec print_list' xs' =
    match xs' with
      [] -> ()
    | [x] -> f x
    | x :: xs'' -> (f x; print_string "; "; print_list' xs'') in
  print_string "[";
  print_list' xs;
  print_string "]"

let sub_list xs start len =
  let rec slice_list' accum xs' i1 i2 =
    match xs' with
      [] -> List.rev accum
    | head :: tail -> 
      if i1 <= 0 && 0 < i2 then 
        slice_list' (head :: accum) tail (i1 - 1) (i2 - 1)
      else if i1 <= 0 then
        slice_list' accum tail (i1 - 1) (i2 - 1)
      else
        List.rev accum in
  slice_list' [] xs start (start + len)
    
let print_grid (f : 'a -> unit) : ('a list list -> unit) = 
  print_list (print_list f)

let add_col_grid (grid : 'a list list) (col : 'a list) : 'a list list = 
  let rec add_col' grid' col' accum = 
    match (grid', col') with
      ([], []) -> List.rev accum
    | (row :: grid'', head :: col'') -> (add_col' grid'' col'' 
                                         ((head :: row) :: accum))
    | _ -> failwith "grid and col not the same length." in
  match (grid, col) with
    ([], _) -> List.map (fun x -> [x]) col
  | _ -> add_col' grid col []
  

let zip_grid (grid : 'a list list) : 'a list list = 
  let rec zip' grid' accum = 
    match grid' with
      [] -> accum
    | row :: grid'' -> zip' grid'' (add_col_grid accum row) in
   zip' grid []
