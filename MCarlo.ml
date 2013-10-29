open Signatures
open Grouper
open Grid

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


(* TODO: increase code reuse. *)

module MakeSwendsenWang = functor (Hist : HISTOGRAM) ->
struct
  type site = bool

  type bond_type = NearestNeighbors | NextNearestNeighbors

  type normalize_type = Random2By2 | InPlace2By2 | InPlace3By3

  type lattice = { g : site G.grid;
                   mutable temp : float }

  type histogram = Hist.histogram

  let int_of_site site = if site then 1 else -1
  let float_of_site site = if site then 1.0 else -1.0
  let int_mag g coord = int_of_site (G.get g coord)
  let float_mag g coord = float_of_site (G.get g coord)
    
  let init width =
    { g = G.init (0, 0) (width - 1, width - 1) (fun _ -> Random.bool ()); 
      temp = 0.0 }

  let site_count lat = (G.width lat.g) * (G.height lat.g)

  let site_energy lat coord = 
    let self_m = int_mag lat.g coord in
    let up_m = int_mag lat.g (G.up lat.g coord) in
    let left_m = int_mag lat.g (G.left lat.g coord) in
    let right_m = int_mag lat.g (G.right lat.g coord) in
    let down_m = int_mag lat.g (G.down lat.g coord) in
    -1.0 *. (float (self_m * (up_m + down_m + left_m + right_m)))

  let set_rand_mag lat nodes =
    let site_type = Random.bool () in
    let coords = List.map (G.coord_of_int lat.g) nodes in
    List.iter (fun coord -> G.set lat.g coord site_type) coords

  let reset lat =
    G.iter G.SequentialSweep (fun coord _ -> G.set lat.g coord true) lat.g;
    lat.temp <- 0.0

  let sweep lat =
    let prob = 1.0 -. exp(-2.0 /. lat.temp) in
    let edges = generate_edges lat.g in
    let edges' = List.filter (fun _ -> Random.float 1.0 < prob) edges in
    let gr = Gr.union ((site_count lat) - 1) edges' in
    List.iter (set_rand_mag lat) (List.map (Gr.group gr) (Gr.group_ids gr))

  let set_temp lat temp = lat.temp <- temp

  let renormalize lat nt param =
    let width = match nt with
        Random2By2 -> (G.width lat.g) / 2
      | InPlace2By2 -> G.width lat.g 
      | InPlace3By3 -> G.width lat.g in
    let setter = match nt with
        Random2By2 -> random2By2_setter
      | InPlace2By2 -> 
        (match param with 
          Some gamma -> inPlace2By2_setter gamma
        | None -> failwith "InPlace2By2 needs a parameter")
      | InPlace3By3 -> 
        (match param with 
          Some gamma -> inPlace3By3_setter gamma
        | None -> failwith "InPlace2By2 needs a parameter") in
    { g = set_new_grid lat.g width setter;
      temp = lat.temp }

  let nn_correlation_func lat sum coord self_mag =
    let right, down = Grid.right lat.g coord, Grid.down lat.g coord in
    sum + self_mag 

  let nnn_correlation_func lat sum coord self_mag = pass

  let correlation lat bt =
    let  correlation_func = (match bt with
        NearestNeighbors -> nn_correlation_func
      | NextNearestNeighbors -> nnn_correlation_func) lat
    in
    float (Grid.foldi correlation_func 0 lat.g)
      
  (* TODO: rewrite this to not suck. *)
  let energy lat =
    G.foldi (fun sum coord _ -> sum +. (site_energy lat coord)) 0.0 lat.g

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
    G.print lat.g (fun s -> if s then print_string "+" else print_string " ")

  let energy_from_hist hist t0 t = t
  let c_from_hist hist t0 t = t
end
