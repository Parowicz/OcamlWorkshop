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
  type element
  type 'a t

  val empty: 'a t
  val any: 'a t -> bool
  val append: element -> 'a t -> 'a t
  val remove: element -> 'a t -> 'a t
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
  
  (**
    @return empty tree
  *)
  let empty = Empty

  (**
    @return true if tree is not empty. 
  *)
  let any = function Empty -> false | _ -> true

  (**
    @return true if element is empty.
  *)
  let is_empty = function Empty -> true | _ -> false

  (** 
    @param element element to append.
    @return new tree with element appended. 
            If tree already contains given element, then same tree will be returned.
  *)
  let rec append element = function 
    |Empty -> Node(element, Empty, Empty)
    |Node(value, left, right) -> 
      if Ord.compare element value <= 0 then Node(value, append element left, right)
      else Node(value, left, append element right)  
  
  (**
    @return tree constructed from list of elements.
  *)
  let from_list lst = List.fold_left (fun acc a -> append a acc) empty lst

  (**
    @return rightmost element of the tree (greatest one).
  *)
  let rec max = function
    |Empty -> None
    |Node(value, _, Empty) -> Some(value)
    |Node(_, _, right) -> max right
  
  (**
    Remove rightmost element from the tree (greatest one).
    
    @return tree without rightmost element.
  *)
  let rec remove_max = function
    |Empty -> Empty
    |Node(_, left, Empty) -> left
    |Node(value, left, right) -> Node(value, left, remove_max right)
  
  (**
    @return leftmost element of the tree (smallest one).
  *)
  let rec min = function 
    |Empty -> None 
    |Node(value, Empty, _) -> Some(value)
    |Node(_, left, _) -> min left
  
  (**
    Remove leftmost element from the tree (smallest one).

    @return tree without leftmost element.
  *)
  let rec remove_min = function 
    |Empty -> Empty
    |Node(_, Empty, right) -> right
    |Node(value, left, right) -> Node(value, remove_min left, right)

  (**
    Remove given element. Replace it with leftmost element in right node. 
    If right node doesn't have left branch, then right node is used instead.
    If removed element doesn't have right node then element is replaced by left node.

    @param element to remove
    @return tree without given element
  *)
  let rec remove element tree = 
    match tree with 
    |Empty -> Empty
    |Node(value, left, Empty) when value = element -> left
    |Node(value, left, right) when value = element 
      -> (match min right with 
          |None -> right
          |Some(minimum) -> Node(minimum, left, remove_min right))
    |Node(value, left, right) -> 
          if Ord.compare element value <= 0 then Node(value, remove element left, right)
          else Node(value, left, remove element right) 
  
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
    |Node(value, left, right) -> flatten left @ value :: flatten right

  (**
    @param lst list of elements with types matches tree type.
    @return sorted list.
  *)
  let sort lst = List.fold_right (fun tree element -> append tree element) lst empty |> flatten
end