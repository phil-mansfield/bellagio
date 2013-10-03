open Array

module type GROUPER =
sig
  type group_record
  type node = int
  type edge = node * node
  type group_id = int
    
  val union : node -> edge list -> group_record
    
  val find : group_record -> node -> group_id
  val group_ids : group_record -> group_id list
    
  val group : group_record -> group_id -> node list
  val group_size : group_record -> group_id -> int
end

(* This is highly non-optimal. Get a hash table up in here at some point. *)
module UncompressedGrouper : GROUPER =
struct 
  type group_record = {ids : int array;
                       sizes : int array;
                       groups : (int list) array;
                       mutable group_ids : int list}
    
  type node = int
  type edge = node * node
  type group_id = int
    
  let rec find g n = 
    if g.ids.(n) = n then n
    else
      let group_id = find g g.ids.(n) in
      g.ids.(n) <- group_id;
      group_id
        
  let union max_node edges =
    let g = { ids = Array.init (max_node + 1) (fun x -> x); 
              sizes = Array.make (max_node + 1) 1;
              groups = Array.make (max_node + 1) [];
              group_ids = [] } in
    
    print_endline "Created initial g.";
    
    let hook (u, v) =
      let u_root, v_root = find g u, find g v in
      if u_root <> v_root then
        let u_size, v_size = g.sizes.(u_root), g.sizes.(v_root) in
        let min_root, max_root = Utils.keyOrder((u_root, u_size), 
                                                (v_root, v_size)) in
        g.ids.(min_root) <- max_root;
        g.sizes.(max_root) <- u_size + v_size; in
    
    let add_group i _ = 
      let root = find g i in
      g.groups.(root) <- i::g.groups.(root);
      if root = i then g.group_ids <- root::g.group_ids; in
    
    List.iter hook edges;
    Array.iteri add_group g.ids;
    g
      
  let group_ids g = g.group_ids
    
  let group g id = g.groups.(id)
    
  let group_size g id = g.sizes.(id)
end
