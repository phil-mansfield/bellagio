(* Dependencies: GroupGraph *)

module type GRID =
  sig
    type 'a grid
    type coord = int * int

    type iter_order = SeqentialSweep | CheckerboardSweep | RandomSweep

    module GroupGraph : GroupGraph.GROUP_GRAPH

    val width : 'a grid -> int
    val height : 'a grid -> int
    
    val get : 'a grid -> coord -> 'a
    val set : 'a grid -> coord -> 'a -> unit

    val make : coord -> coord -> 'a -> 'a grid
    val init : coord -> coord -> (int -> 'a) -> 'a grid

    val iter : 'a grid -> iter_order -> ('a -> 'a) -> unit
    val iteri : 'a grid -> iter_order -> (coord -> 'a -> 'a) -> unit
    val map : 'a grid -> ('a -> 'b) -> 'b grid
    val fold : 'a grid -> 'a -> ('a * 'a -> 'a) -> 'a

    val union : 'a grid -> coord GroupGraph.group_record 
  end
