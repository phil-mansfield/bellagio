val id : 'a -> 'a

val valOrder : ('a * 'a) -> ('a * 'a)
val keyOrder : (('a * 'a) * ('a * 'a)) -> ('a * 'a)

(* list functions *)

val tabulate_list : (int -> 'a) -> int -> 'a list
val enumerate_list : 'a list -> (int * 'a) list
val print_list : ('a -> unit) -> 'a list -> unit
val sub_list : 'a list -> int -> int -> 'a list

(* grid functions *)

val add_col_grid : 'a list list -> 'a list -> 'a list list
val zip_grid : 'a list list -> 'a list list
val print_grid : ('a -> unit) -> 'a list list -> unit
