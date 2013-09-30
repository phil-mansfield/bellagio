open Array

type group_record = {ids : int array;
                     sizes : int array}
type node = int
type edge = node * node
type group_id = int
      
let union max_node edges =
  raise (Failure "Not Yet Implemented")
    
let rec find g n = 
  if g.ids.(n) = n then n
  else
    let group_id = find g n in
    let _ = g.ids.(n) <- group_id in
    group_id
    
let group_ids g =
  raise (Failure "Not Yet Implemented")
    
let group g id = 
  raise (Failure "Not Yet Implemented")
    
let group_edges g id =
  raise (Failure "Not Yet Implemented")
    
let group_size g id =
  raise (Failure "Not Yet Implemented")
