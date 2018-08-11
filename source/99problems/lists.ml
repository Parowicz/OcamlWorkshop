(* Problem 1*)
let rec last = function
  |[] -> None
  |[x] -> Some(x)
  |_ :: xs -> last xs
;;

(* Problem 2*)
let rec last_two = function
  |[] | [_] -> None
  |x :: y :: [] -> Some(x, y)
  |_ :: xs -> last_two xs
;;

(* Problem 3*)
let rec nth index lst = 
  match (index, lst) with
  |(1, x :: _) -> Some(x)
  |(_, []) -> None
  |(n, _ :: xs) -> nth (n - 1) xs
;;

(* Problem 4*)
let length lst = 
  let rec aux acc = function 
    |[] -> acc
    |x :: xs -> aux (acc + 1) xs
  in aux 0 lst
;; 

(* Problem 5*)
let reverse lst = 
  let rec aux acc = function
    |[] -> acc
    |x :: xs -> aux (x :: acc) xs
  in aux [] lst
;;

(* Problem 6*)
let is_polindrome lst = lst = (reverse lst)
;;

type 'a node = 
  |One of 'a
  |Many of 'a node list
;;

(* Problem 7*)
let rec flatten = function
  |[] -> []
  |One(element) :: xs -> element :: flatten xs
  |Many(elements) :: xs -> flatten elements @ flatten xs
;;

(* Problem 8*)
let rec compress = function
  |x1 :: (x2 :: _ as rest) -> 
    if x1 = x2 then compress rest
    else x1 :: compress rest
  |l -> l
;; 

(** Split list when given condition not matched *)
let split_when f lst = 
  let rec aux acc = function
    |[] -> (acc, [])
    |x :: xs as all -> if f x then aux (acc @ [x]) xs
                else (acc, all)
  in aux [] lst
;;

(* Problem 9*)
let rec pack = function
  |[] -> []
  |lst -> let group, rest = split_when (fun x -> x = List.hd lst) lst
          in match rest with
             |[] -> [group]
             |l -> group :: pack rest
;;

(* Problem 10*)
let encode lst = 
  pack lst 
  |> List.map (fun x -> (List.length x, List.hd x))
;;