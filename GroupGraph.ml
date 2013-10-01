open Array

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
    let group_id = find g n in
    g.ids.(n) <- group_id;
    group_id

let union max_node edges =
  let g = { ids = Array.init max_node (fun x -> x); 
            sizes = Array.make max_node 1;
            groups = Array.make max_node [];
            group_ids = [] } in

  let hook (u, v) =
    let uRoot, vRoot = find g u, find g v in
    if uRoot <> vRoot then
      let uSize, vSize = g.sizes.(u), g.sizes.(v) in
      let minRoot, maxRoot = Utils.keyOrder((uRoot, uSize), (vRoot, vSize)) in
      g.ids.(minRoot) <- maxRoot;
      g.sizes.(maxRoot) <- uSize + vSize; in

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
