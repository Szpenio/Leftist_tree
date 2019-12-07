(** type representing joinable priority queue **)
type 'a queue

(** empty priority queue **)
val empty : 'a queue

(** adds element [e] to the queue [q] **)
val add : 'a -> 'a queue -> 'a queue

(** expection raised in case that queue is empty **)
exception Empty

(** removes from the queue element of the lowest priority **)
val delete_min : 'a queue -> 'a * 'a queue

(** joins queues [q1], [q2] **)
val join : 'a queue -> 'a queue -> 'a queue

(** checks whether queue is empty **)
val is_empty : 'a queue -> bool
