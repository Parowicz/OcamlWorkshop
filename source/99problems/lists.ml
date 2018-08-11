(**
  List based problems from 99 problems. This module aims to solve them in functional way.

  @author Artur P.
  @version 0.0
*)

(** 
  Problem 1
  @return Some(last element of list) or None if list is empty.
*)
let rec last = function
  |[] -> None
  |[x] -> Some(x)
  |_ :: xs -> last xs
;;

(** Problem 2
  @return Some(last two elements of list) or None if list doesn't have 2 elements.
*)
let rec last_two = function
  |[] | [_] -> None
  |x :: y :: [] -> Some(x, y)
  |_ :: xs -> last_two xs
;;

(** Problem 3
  @param index Index of the element. Counting from {i 1}.
  @return Some(element at given index) or None if index is out of range.
*)
let rec nth index lst = 
  match (index, lst) with
  |(1, x :: _) -> Some(x)
  |(_, []) -> None
  |(n, _ :: xs) -> nth (n - 1) xs
;;

(** Problem 4
  @return length of list as int
*)
let length lst = 
  let rec aux acc = function 
    |[] -> acc
    |x :: xs -> aux (acc + 1) xs
  in aux 0 lst
;; 

(** Problem 5
  @return new list created by reverse of given list.
*)
let reverse lst = 
  let rec aux acc = function
    |[] -> acc
    |x :: xs -> aux (x :: acc) xs
  in aux [] lst
;;

(** Problem 6
  @return True if list is a palindrome, false otherwise.
*)
let is_palindrome lst = lst = (reverse lst)
;;

(**
  Simple tree representation. Each node may contain value of certain type or other tree.
*)
type 'a node = 
  |One of 'a (** Single value *)
  |Many of 'a node list (** Another tree *)
;;

(** Problem 7 
  Traverse given tree and create list with each element visited, step by step.

  @return 1-dimensional list created from flatting given tree.
*)
let rec flatten = function
  |[] -> []
  |One(element) :: xs -> element :: flatten xs
  |Many(elements) :: xs -> flatten elements @ flatten xs
;;

(** Problem 8
  Eliminate consecutive duplicates of list elements.
  @return New list, without consecutive duplicates.
*)
let rec compress = function
  |x1 :: (x2 :: _ as rest) -> 
    if x1 = x2 then compress rest
    else x1 :: compress rest
  |l -> l
;; 

(**
Split given list at the momemnt when given condition is not matched. 

@param f Condition, point where this condition return false is split point.
@return Tuple with list of consecutive maches with f as first element and rest as second.
If given list is empty, both elements of tuple will be empty. 
*)
let split_when f lst = 
  let rec aux acc = function
    |[] -> (acc, [])
    |x :: xs as all -> if f x then aux (acc @ [x]) xs
                else (acc, all)
  in aux [] lst
;;

(** Problem 9
Pack consecutive duplicates of list elements into sublists.

@return 2-dimensional list with grouped, consecutive elements.
*)
let rec pack = function
  |[] -> []
  |x :: _ as lst -> let group, rest = split_when (fun x1 -> x1 = x) lst
                    in match rest with
                       |[] -> [group]
                       |l -> group :: pack rest
;;

(** Problem 10
Run-length encoding of a list. Consecutive elements are encoded as tuple with 
(number of occcourences, element). 

@return List of tuples with (number of occcourences as int, 'a element)
*)
let rec encode = function
  |[] -> []
  |x :: _ as lst -> let rec aux e acc = function
                        |[] -> (acc, [])
                        |x :: xs as all -> if e = x then aux e (acc + 1) xs
                                  else (acc, all)
                    in let  ctr, rest = aux x 0 lst
                    in (ctr, x) :: encode rest
  (* Note: that's an ugly solution, similar to pack. 
    Previously List.map (fun x -> (List.length x, List.hd x)) was used. 
    Thous this version is more efficient
    *)
;;
