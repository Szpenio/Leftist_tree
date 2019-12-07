(** Project: Leftist trees **)
(** Autor: Antoni Koszowski **)
(** Reviewer: Jakub Szulc  **)

(** type representing joinable priority queue; nlp - is the length of the most right path **)
type 'a queue = Node of { prio: 'a; lewy: 'a queue; prawy: 'a queue; nlp: int } | Null

(** empty priority queue **)
let empty = Null

(** checks whether [q] is empty **)
let is_empty q =
  if q = Null then true else false

(** joins queues [q1], [q2] **)
let join q1 q2 =
  match q1, q2 with
    Null, _ -> q2
  | _, Null -> q1
  | _ ->
    let rec merge q1 q2 =
      (** maintains heap property, determines nlp of father depending on his descendants **)
      let fix (Node father) (Node cand_l) (Node cand_p) =
        if cand_l.nlp >= cand_p.nlp then Node {father with lewy = (Node cand_l); prawy = (Node cand_p); nlp = cand_p.nlp + 1}
        else
          Node {father with lewy = (Node cand_p); prawy = (Node cand_l); nlp = cand_l.nlp}
      in let proc (Node q1) (Node q2) =
           match q1.lewy, q1.prawy with
             Null, Null -> Node {q1 with lewy = (Node q2); nlp = 0}
           | _, Null -> fix (Node q1) q1.lewy (Node q2)
           | _, _-> fix (Node q1) q1.lewy (merge q1.prawy (Node q2))
      (** determines the queue, which root has a fewer priority **)
      in let choose (Node q1) (Node q2) =
           if q1.prio <= q2.prio then proc (Node q1) (Node q2)
         else
           proc (Node q2) (Node q1)
      in choose q1 q2
    in merge q1 q2

(** adds element [e] to the queue [q] **)
let add e q =
  join (Node {prio = e; lewy = Null; prawy = Null; nlp = 0}) q

(** expection raised in case that queue is empty **)
exception Empty

(** removes from the queue element of the lowest priority **)
let delete_min = function
  Null -> raise Empty
  | (Node q) -> (q.prio, join q.lewy q.prawy)
