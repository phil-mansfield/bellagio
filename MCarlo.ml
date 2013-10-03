open Signatures

module MakeSwendsenWang = functor (Hist : HISTOGRAM) ->
struct
  type lattice = unit
  type histogram = Hist.histogram

  let init width =
    failwith "Not Yet Implemented"

  let site_count lat =
    failwith "Not Yet Implemented"

  let sweep lat =
    failwith "Not Yet Implemented"

  let set_temp lat temp =
    failwith "Not Yet Implemented"

  let energy lat = 
    failwith "Not Yet Implemented"

  let magnetization lat =
    failwith "Not Yet Implemented"

  let max_cluster_size lat = 
    failwith "Not Yet Implemented"
    
  let create_histogram lat sweeps =
    failwith "Not Yet Implemented"

  let print lat = 
    failwith "Not Yet Implemented"
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

  let site_energy h coord = 
    let self_m = int_mag h.g coord in
    let up_m = int_mag h.g (G.up h.g coord) in
    let left_m = int_mag h.g (G.left h.g coord) in
    let right_m = int_mag h.g (G.right h.g coord) in
    let down_m = int_mag h.g (G.down h.g coord) in
    -1.0 *. (float (self_m * (up_m + down_m + left_m + right_m)))
    
  let init width =
    {g = G.make (0, 0) (width, width) true; temp = 0.0; 
     energy = -2.0 *. (float (width * width));  
     mag = 1.0 *. (float (width * width))}

  let site_count lat = (G.width lat.g) * (G.height lat.g)

  let sweep lat =
    failwith "Not Yet Implemented"

  let set_temp lat temp = lat.temp <- temp

  let energy lat = lat.energy

  let magnetization lat = lat.mag

  let max_cluster_size lat = 
    failwith "Not Yet Implemented"
    
  let create_histogram lat sweeps =
    failwith "Not Yet Implemented"

  let print lat = 
    failwith "Not Yet Implemented"
end
