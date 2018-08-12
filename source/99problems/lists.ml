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
  @return true if list is a palindrome, false otherwise.
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
  |Many(elements) :: xs -> flatten (elements @ xs) 
;;

(** Problem 8
  Eliminate consecutive duplicates of list elements.
  @return new list, without consecutive duplicates.
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
@return tuple with list of consecutive maches with f as first element and rest as second.
If given list is empty, both elements of tuple will be empty. 
*)
let split_when f lst = 
  let rec aux acc = function
    |[] -> (reverse acc, [])
    |x :: xs as all -> if f x then aux (x :: acc) xs
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

@return list of tuples with (number of occcourences as int, 'a element)
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

(**
  This type represents run-length encoding segment.
*)
type 'a rle = 
  |Single of 'a (** Single element *)
  |Multiple of int * 'a (** Repeated element, tuple with (number of repetitions, element) *)
;;

(** Problem 11
Run-length encoding to rle list.

@return encoded list.
*)
let encode_rle lst = 
  encode lst 
  |> List.map (function (1, x) -> Single(x) | (n, x) -> Multiple(n, x))
;;

(**
Create list by repeating given element n times.

@param number of repetitions
@param element to repeat
@return list with element repeated n times.
*)
let repeat number element = 
  let rec aux acc = function
    |n when n < 0 -> failwith "Negative number passed as number of repeats"
    |0 -> acc
    |n -> aux (element :: acc) (n - 1)
  in aux [] number
;;

(** Problem 12
Decode list of 'a rle elements into list of 'a elements.

@return decoded list.
*)
let decode_rle lst = 
  let rec aux acc = function
    |[] -> reverse acc
    |Single(element) :: xs -> aux (element :: acc) xs
    |Multiple(count, element) :: xs -> aux (repeat count element @ acc) xs
  in aux [] lst
;;

(* Problem 13 - from my understanding decode_rle fits requirements *)

(** Problem 14
@return list with each element duplicated. 
*)
let duplicate lst = 
  let rec aux acc = function
    |[] -> reverse acc
    |x :: xs -> aux (x :: x :: acc) xs
  in aux [] lst
;;

(** Problem 15
@param n defines how many times each element has to be replicated.
@return list with each element replicated n times.
*)
let replicate lst n = 
  let rec aux acc a = function
    |[] -> reverse acc
    |x :: xs as l -> if a = 0 then aux acc n xs
                     else aux (x :: acc) (a - 1) l 
  in aux [] n lst 
;;

(** Problem 16
@param n dropping step.
@return list without elements that index mod n is equal 0. 
  If n is equal to one, empty list will be returned. 
  If n is greater than list length same list as passed will be returned.
@raise Failure "Index <= 0" when n is lesser or equal to 0.
*)
let drop lst n = 
  if n <= 0 then failwith "Index <= 0" else
  let rec aux index acc accl = 
    match index mod n, acc with
    |(_, []) -> List.rev accl
    |(0, _ :: xs) -> aux (index + 1) xs accl
    |(_, x :: xs) -> aux (index + 1) xs (x :: accl)
  in aux 1 lst []
;;

(** Problem 17
Split a list into two parts

@param n length of first part
@return tuple with (first part, second part).
  If n is out of list range tuple of (whole list, empty list) will be returned.
*)
let split lst n = 
  let rec aux acc ctr accl = 
    match ctr, accl with 
    |(_, []) -> (reverse acc, [])
    |(0, xs) -> (reverse acc, xs)
    |(n, x :: xs) -> aux (x :: acc) (n - 1) xs
  in aux [] n lst
;;

(** Problem 18
@param start_i starting index.
@param end_i ending index.
@return sublist with all elements between start_i - end_i.
    If start_i is lesser than 0 it will be treated as 0.
    If end_i is larger than list length it will be treated as list length.
*)
let slice lst start_i end_i =
  let rec aux acc i = function
    |[] -> reverse acc
    |_ when i > end_i + 1 -> reverse acc 
    |x :: xs when i > start_i -> aux (x :: acc) (i + 1) xs
    |_ :: xs -> aux acc (i + 1) xs
  in aux [] 1 lst
;;

(** Problem 19
@param n step 
@return list rotated to left n times. 
  If n is negative, then it will treated as length + n.
*)
let rotate lst n = 
  let len = length lst
  in let l, r = split lst (if n >= 0 then n else len + n)
  in r @ l
;;
