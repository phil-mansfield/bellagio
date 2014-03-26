open Signatures
open Grouper
open Grid

open Array

module G = FlatGrid
module Gr = UncompressedGrouper

let generate_edges (g : bool G.grid) : (int * int) list = 
  let add_edge edges c1 c2 = 
    (G.int_of_coord g c1, G.int_of_coord g c2) :: edges in
  let add_edges edges c site =
    let right_c, down_c = G.right g c, G.down g c in 
    let right_site, down_site = G.get g right_c, G.get g down_c in
    let edges' = if right_site = site then add_edge edges right_c c else edges in
    if down_site = site then add_edge edges' down_c c else edges' in
  G.foldi add_edges [] g
;;


let set_new_grid src width setter =
  let g' = G.make (0, 0) (width - 1, width - 1) true in
  G.iter G.SequentialSweep (setter src g') g'; g'
;;
  
let random2By2_setter src target (x, y) _ =
  let cells = [(x * 2, y * 2); (x * 2 + 1, y * 2); 
               (x * 2, y * 2 + 1); (x * 2 + 1, y * 2 + 1)] in
  let mags = List.map (fun c -> if G.get src c then 1 else -1) cells in
  let avg = List.fold_left (fun a b -> a + b) 0 mags in
  G.set target (x, y) (avg > 0 || (avg = 0 && (Random.int 2) = 1))
;;

let inPlace2By2_setter gamma src target c _ =
  let right, up = G.right src, G.up src in
  let cells = [c; right c; up c; right (up c)] in
  let mags = List.map (fun c -> if G.get src c then 1.0 else -.1.0) cells in
  let avg = List.fold_left (fun x y -> x +. y) 0.0 mags in
  G.set target c ((Random.float 1.0) < 1.0 /. (1.0 +. exp(-.gamma *. avg)))
;;

let inPlace3By3_setter gamma src target c _ =
  let r, u, l, d = G.right src, G.up src, G.left src, G.down src in
  let cells = [d (l c); d c; d (r c); l c; c; r c; u (l c); u c; u (r c);] in
  let mags = List.map (fun c -> if G.get src c then 1.0 else -.1.0) cells in
  let avg = List.fold_left (fun x y -> x +. y) 0.0 mags in
  G.set target c ((Random.float 1.0) < 1.0 /. (1.0 +. exp(-.gamma *. avg)))
;;

let int_of_site site = 
  if site then 1 else -1
;;

let float_of_site site = 
  if site then 1.0 else -1.0
;;

module MakeSwendsenWang = functor (Hist : HISTOGRAM) ->
struct
  type site = bool
  type bond_type = NN1 | NN2 | NN3 | DMD | SQR | MAG
  type normalize_type = Random2By2 | InPlace2By2 | InPlace3By3

  type lattice = { g : site G.grid;
                   mutable temp : float;
                   mutable bonds : (bond_type * float) array}

  type histogram = Hist.histogram
  
  let bond_relation bond g coord = 
    match bond with
    | NN1 -> 
        [|[|G.left g coord; coord|]; 
          [|G.down g coord; coord|]|]
    | NN2 -> 
      let l = G.left g coord in 
      [|[|G.up g l; coord|]; [|G.down g l; coord|]|]
    | NN3 -> 
      [|[|G.left g (G.left g coord); coord|]; 
        [|G.down g (G.down g coord); coord|]|]
    | DMD -> 
      [|[|G.up g coord; G.left g coord; 
          G.right g coord; G.down g coord|]|]
    | SQR -> 
      let l =  G.left g coord in 
      [|[|coord; l; G.down g coord; G.down g l|]|]
    | _ -> failwith "Cannot call bond_relation on this bond type."

  let bond_locks bt g coord = 
    let bond_sets = bond_relation bt g coord in
    let matches = Array.make (Array.length bond_sets) 0 in
    for bond_set_i=0 to Array.length bond_sets - 1 do
      let bond_set = bond_sets.(bond_set_i) in
      let all_same = ref true in
      for i=1 to Array.length bond_set - 1 do
        (* This is slightly sub-optimal: *)
        all_same := !all_same && (G.get g bond_set.(i-1) = G.get g bond_set.(i))
      done;
      if !all_same then matches.(bond_set_i) <- 1;
    done;
    matches

  let bond_correlation bt g coord =
    Array.fold_left (fun x y -> x + y) 0 (bond_locks bt g coord)

  let bond_edges bt prob g coord = 
    let bond_sets = bond_relation bt g coord in
    (* Goddammit, OCaml, even Go does this one correctly >:( *)
    let edges : (int * int) list ref = ref [] in
    let locks = bond_locks bt g coord in
    for bond_set_i=0 to Array.length bond_sets - 1 do
      (* Think hard about union-find to figure out whether or not this
         appoach gives optimal branching: *)
      if locks.(bond_set_i) <> 0 &&  Random.float 1.0 < prob then
        let bond_set = bond_sets.(bond_set_i) in
        for i=1 to Array.length bond_set - 1 do
          let edge = (G.int_of_coord g bond_set.(i-1), 
                      G.int_of_coord g bond_set.(i)) in
          let (u, v) = edge in
          edges := edge :: !edges
        done;
    done;
    !edges

  let int_mag g coord = int_of_site (G.get g coord)
  let float_mag g coord = float_of_site (G.get g coord)
    
  let init width =
    { g = G.init (0, 0) (width - 1, width - 1) (fun _ -> Random.bool ()); 
      temp = 0.0;
      bonds = [|(NN1, 1.0)|] }

  let site_count lat = (G.width lat.g) * (G.height lat.g)
    
  let set_rand_mag lat nodes =
    let site_type = Random.bool () in
    let coords = List.map (G.coord_of_int lat.g) nodes in
    List.iter (fun coord -> G.set lat.g coord site_type) coords

  let reset lat =
    G.iter G.SequentialSweep
      (fun coord _ -> G.set lat.g coord (Random.bool ())) lat.g

  let sweep lat =
    let global_edges : (int * int) list list ref = ref [] in
    for i=0 to Array.length lat.bonds - 1 do
      let (bt, k) = lat.bonds.(i) in
      let prob = 1.0 -. exp(-1.0 *. k /. lat.temp) in
      let edges = List.flatten 
        (G.foldi (fun xs c _ -> bond_edges bt prob lat.g c :: xs) [] lat.g) in
      global_edges := edges :: !global_edges;
    done;
    let gr = Gr.union (site_count lat - 1) (List.flatten !global_edges) in
    List.iter (set_rand_mag lat) (List.map (Gr.group gr) (Gr.group_ids gr))

  let set_temp lat temp = lat.temp <- temp

  let set_bond_types lat bonds = lat.bonds <- bonds

  let renormalize lat nt param =
    let width = match nt with
      | Random2By2 -> (G.width lat.g) / 2
      | InPlace2By2 -> G.width lat.g 
      | InPlace3By3 -> G.width lat.g in
    let setter = match nt with
      | Random2By2 -> random2By2_setter
      | InPlace2By2 -> 
        (match param with 
        | Some gamma -> inPlace2By2_setter gamma
        | None -> failwith "InPlace2By2 needs a parameter")
      | InPlace3By3 -> 
        (match param with 
        | Some gamma -> inPlace3By3_setter gamma
        | None -> failwith "InPlace3By3 needs a parameter") in
    { g = set_new_grid lat.g width setter;
      temp = lat.temp;
      bonds = lat.bonds }

  (* TODO: Figure out if you need to divide by n. *)
  let correlation lat bt =
    match bt with
    | MAG ->
      float (G.fold (fun sum site -> sum + (int_of_site site)) 0 lat.g)
    | _ -> 
      let corr_func = bond_correlation bt in
      float (G.foldi (fun sum coord _ -> sum + (corr_func lat.g coord)) 0 lat.g)

  let energy lat =
    let energy = ref 0. in
    for i=0 to Array.length lat.bonds - 1 do
      let (bt, k) = lat.bonds.(i) in
      energy := !energy +. k *. (correlation lat bt);
    done;
    -1. *. !energy

  let magnetization lat = 
    G.foldi (fun sum coord _ -> sum +. (float_mag lat.g coord)) 0.0 lat.g
    
  let max_cluster_size lat = 
    let record = Gr.union (site_count lat) (generate_edges lat.g) in
    Gr.group_size record (Gr.largest_group record)

  let create_histograms lat sweeps =
    let e_hist = Hist.init (-2.0 *. (float (site_count lat))) 
      2.0 (1 + 2 * site_count lat) in 
    let m_hist = Hist.init (-1.0 *. (float (site_count lat))) 
      1.0 (1 + 2 * site_count lat) in 
    for sweep_num = 1 to sweeps do 
      sweep lat;
      Hist.add e_hist (energy lat);
      Hist.add m_hist (magnetization lat);
    done;
    (e_hist, m_hist)
      
  let print lat = 
    G.print lat.g (fun s -> if s then print_string "#" else print_string " ")

  let energy_from_hist hist t0 t =
    failwith "energy_from_hist not yet mplemented"
  let c_from_hist hist t0 t = 
    failwith "c_from_hist not yet mplemented"
end
