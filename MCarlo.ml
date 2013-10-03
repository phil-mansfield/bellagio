module MakeSwendsenWang = functor(Hist : Signatures.HISTOGRAM) ->
struct
  type lattice = unit
  type histogram = Hist.histogram

  let init width initial_temp =
    failwith "Not Yet Implemented"

  let sweep lat =
    failwith "Not Yet Implemented"

  let set_temp lat =
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

module MakeMetropolis = functor(Hist : Signatures.HISTOGRAM) ->
struct
  type lattice = unit
  type histogram = Hist.histogram

  let init width initial_temp =
    failwith "Not Yet Implemented"

  let sweep lat =
    failwith "Not Yet Implemented"

  let set_temp lat =
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
