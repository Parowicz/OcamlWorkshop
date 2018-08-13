(** 
  Binary Tree implementation 
  @author Artur P.
  @version 0.0
*)

(** Abstraction describing type that supports ordering. *)
module type OrderedType =
  sig
    type t

    val compare: t -> t -> int
  end

(** Binary tree signature. It must implement types representing single value and whole tree. *)
module type S = sig
  (** Type of values stored in tree. *)
  type element
  
  (** Type of whole tree *)
  type 'a t

  (** @return empty tree *)
  val empty: 'a t

  (** @return true if tree is not empty. Otherwise false. *)
  val any: 'a t -> bool

  (** @return tree appended with given element. *)
  val append: element -> 'a t -> 'a t

  (** @return true if tree contains given element. *)
  val contains: element -> 'a t -> bool
end

(** Create binary tree based on orderable value. *)
module Make (Ord: OrderedType) = struct
  (** Element type must be comparable. *)
  type element = Ord.t

  (** Represent tree as union of Empty element and Node. *)
  type 'a t = 
    |Empty (** End of tree. *)  
    |Node of element * 'a t * 'a t (** Node has greater value on right side and lesser on left. *)
  
  let empty = Empty
  let any = function Empty -> false | _ -> true
  let is_empty = function Empty -> true | _ -> false

  (** 
    @param element element to append.
    @return new tree with element appended. 
            If tree already contains given element, then same tree will be returned.
  *)
  let rec append element = function 
    |Empty -> Node(element, Empty, Empty)
    |Node(value, left, right) -> 
      if Ord.compare element value <= 0 then Node(value,  append element left, right)
      else Node(value, left, append element right)  
  
  (** 
    @param element element to look for.
    @return true if tree contains given element.
  *)
  let rec contains element = function
    |Empty -> false
    |Node(value, _, _) when value = element -> true
    |Node(value, left, right) ->
      if Ord.compare element value > 0 then contains element right
      else contains element left

  (**
    @return list of elements from tree flattens in left-right order.
  *)
  let rec flatten = function
    |Empty -> []
    |Node(value, l, r) -> flatten l @ value :: flatten r

  (**
    @param lst list of elements with types matches tree type.
    @return sorted list.
  *)
  let sort lst = List.fold_right (fun tree ele -> append tree ele) lst empty |> flatten

end
