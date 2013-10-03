module type GRID = 
sig
  type 'a grid
  type coord = int * int

  type iter_order = SequentialSweep | CheckerboardSweep | RandomSweep
      
  val width : 'a grid -> int
  val height : 'a grid -> int
    
  val get : 'a grid -> coord -> 'a
  val set : 'a grid -> coord -> 'a -> unit
    
  val make : coord -> coord -> 'a -> 'a grid
  val init : coord -> coord -> (coord -> 'a) -> 'a grid
    
  val iter : iter_order -> (coord -> 'a -> unit) -> 'a grid -> unit
  val fold : iter_order -> ('a -> 'b -> 'a) -> 'a -> 'b grid -> 'a

  val print : 'a grid -> ('a -> unit) -> unit

  val right : 'a grid -> coord -> coord
  val left : 'a grid -> coord -> coord
  val up : 'a grid -> coord -> coord
  val down : 'a grid -> coord -> coord
end

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

module type HISTOGRAM =
sig
  type histogram
  
  val init : float -> float -> int -> histogram
  val init_bounded : float -> float -> int -> histogram

  val bin_value : histogram -> int -> float
  val bin_index : histogram -> float -> int

  val add : histogram -> float -> unit
  val bins : histogram -> int list
end

module type MCARLO = 
sig
  type lattice
  type histogram

  val init : int -> lattice
  val site_count : lattice -> int

  val sweep : lattice -> unit
  val set_temp : lattice -> float -> unit

  val energy : lattice -> float
  val magnetization : lattice -> float
  val max_cluster_size : lattice -> int
    
  val create_histograms : lattice -> int -> (histogram * histogram)

  val print : lattice -> unit
end

module type MCARLO_FUNCTOR = functor (Hist : HISTOGRAM) ->
    MCARLO with type histogram = Hist.histogram
