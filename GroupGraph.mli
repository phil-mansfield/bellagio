type group_record
type node = int
type edge = node * node
type group_id = int
      
val union : node -> edge list -> group_record

val find : group_record -> node -> group_id
val group_ids : group_record -> group_id list

val group : group_record -> group_id -> node list
val group_size : group_record -> group_id -> int
