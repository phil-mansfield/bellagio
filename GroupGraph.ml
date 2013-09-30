type 'a group_record = {ids : int Array.array;
                        sizes : int Array.array}
type node = int
type edge = node * node
type group_id = int
      
let union max_node edges =
  let ids = Array.init max_node (fn x -> x)
    
let find g = 
  raise (Failure "Not Yet Implemented")
    
let group_ids g =
  raise (Failure "Not Yet Implemented")
    
let group g id = 
  raise (Failure "Not Yet Implemented")
    
let group_edges g id =
  raise (Failure "Not Yet Implemented")
    
let group_size g id =
  raise (Failure "Not Yet Implemented")
