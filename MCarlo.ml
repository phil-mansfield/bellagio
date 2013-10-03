open Signatures

(* TODO: increase code reuse. *)

module MakeSwendsenWang = functor (Hist : HISTOGRAM) ->
struct
  module G = Grid.FlatGrid

  type site = bool
  type lattice = { g : site G.grid;
                   mutable temp : float;
                   mutable energy : float;
                   mutable mag : float }

  type histogram = Hist.histogram

  let int_of_site site = if site then 1 else -1
  let float_of_site site = if site then 1.0 else -1.0
  let int_mag g coord = int_of_site (G.get g coord)
  let float_mag g coord = float_of_site (G.get g coord)
    
  let init width =
    {g = G.make (0, 0) (width, width) true; temp = 0.0; 
     energy = -2.0 *. (float (width * width));  
     mag = 1.0 *. (float (width * width))}

  let site_count lat = (G.width lat.g) * (G.height lat.g)

  let site_energy lat coord = 
    let self_m = int_mag lat.g coord in
    let up_m = int_mag lat.g (G.up lat.g coord) in
    let left_m = int_mag lat.g (G.left lat.g coord) in
    let right_m = int_mag lat.g (G.right lat.g coord) in
    let down_m = int_mag lat.g (G.down lat.g coord) in
    -1.0 *. (float (self_m * (up_m + down_m + left_m + right_m)))

  let update_site lat coord _ =
    let d_e = -2.0 *. (site_energy lat coord) in
    (* TODO: optimize this calculation *)
    if d_e < 0. || Random.float 1. < exp (d_e /. lat.temp) then begin
      G.set lat.g coord (not (G.get lat.g coord));
      lat.energy <- lat.energy +. d_e;
      lat.mag <- lat.mag -. 2.0 *. (float_mag lat.g coord)
    end

  let sweep lat = G.iter G.SequentialSweep (update_site lat) lat.g

  let set_temp lat temp = lat.temp <- temp

  let energy lat = lat.energy

  let magnetization lat = lat.mag
    
  let max_cluster_size lat = 
    failwith "Not Yet Implemented"
      
  let create_histograms lat sweeps =
    let e_hist = Hist.init (-4.0 *. (float (site_count lat))) 
      4.0 (1 + 2 * site_count lat) in 
    let m_hist = Hist.init (-1.0 *. (float (site_count lat))) 
      1.0 (1 + 2 * site_count lat) in 
    for sweep_num = 1 to sweeps do 
      sweep lat;
      Hist.add e_hist lat.energy;
      Hist.add m_hist lat.mag;
    done;
    (e_hist, m_hist)
      
  let print lat = 
    G.print lat.g (fun s -> if s then print_string "+" else print_string "-")
end

module MakeMetropolis = functor (Hist : HISTOGRAM) ->
struct
  module G = Grid.FlatGrid

  type site = bool
  type lattice = { g : site G.grid;
                   mutable temp : float;
                   mutable energy : float;
                   mutable mag : float }

  type histogram = Hist.histogram

  let int_of_site site = if site then 1 else -1
  let float_of_site site = if site then 1.0 else -1.0
  let int_mag g coord = int_of_site (G.get g coord)
  let float_mag g coord = float_of_site (G.get g coord)
    
  let init width =
    {g = G.make (0, 0) (width, width) true; temp = 0.0; 
     energy = -2.0 *. (float (width * width));  
     mag = 1.0 *. (float (width * width))}

  let site_count lat = (G.width lat.g) * (G.height lat.g)


  let site_energy lat coord = 
    let self_m = int_mag lat.g coord in
    let up_m = int_mag lat.g (G.up lat.g coord) in
    let left_m = int_mag lat.g (G.left lat.g coord) in
    let right_m = int_mag lat.g (G.right lat.g coord) in
    let down_m = int_mag lat.g (G.down lat.g coord) in
    -1.0 *. (float (self_m * (up_m + down_m + left_m + right_m)))

  let update_site lat coord _ =
    let d_e = -2.0 *. (site_energy lat coord) in
    (* TODO: optimize this calculation *)
    if d_e < 0. || Random.float 1. < exp (d_e /. lat.temp) then begin
      G.set lat.g coord (not (G.get lat.g coord));
      lat.energy <- lat.energy +. d_e;
      lat.mag <- lat.mag -. 2.0 *. (float_mag lat.g coord)
    end

  let sweep lat = G.iter G.SequentialSweep (update_site lat) lat.g

  let set_temp lat temp = lat.temp <- temp

  let energy lat = lat.energy

  let magnetization lat = lat.mag

  let max_cluster_size lat = 
    failwith "Not Yet Implemented"
    
  let create_histograms lat sweeps =
    let e_hist = Hist.init (-4.0 *. (float (site_count lat))) 
      4.0 (1 + 2 * site_count lat) in 
    let m_hist = Hist.init (-1.0 *. (float (site_count lat))) 
      1.0 (1 + 2 * site_count lat) in 
    for sweep_num = 1 to sweeps do 
      sweep lat;
      Hist.add e_hist lat.energy;
      Hist.add m_hist lat.mag;
    done;
    (e_hist, m_hist)

  let print lat = 
    G.print lat.g (fun s -> if s then print_string "+" else print_string "-")
end
