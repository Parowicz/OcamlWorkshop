type 'a t = Empty | Node of 'a * 'a t * 'a t * 'a t * bool

let empty = Empty;;

let singleton x = Node(x, Empty, Empty, Empty, true);;

let add elements tree = 
  let rec aux acc a =
    match acc with 
    |[] -> a 
    |x :: xs as lst ->
      let last = match xs with [] -> true | _ -> false 
      in match a with
         |Empty -> aux lst (Node(x, Empty, Empty, Empty, last))
         |Node(value, left, center, right, is_end) ->
                if x = value then Node(value, left, aux xs center, right, is_end || last)
                else if x < value then Node(value, aux lst left, center, right, is_end || last)
                else Node(value, left, center, aux lst right, is_end || last)
  in aux elements tree
;;

let contains elements tree = 
  let rec aux elements = function
    |Empty -> false
    |Node(value, left, center, right, is_end) ->
      match elements with 
      |[] -> false
      |[element] as lst -> 
        if element = value then is_end
        else if element < value then aux lst left
        else aux lst right 
      |element :: rest -> 
        if element = value then aux rest center
        else if element < value then aux rest left
        else aux rest right 
  in aux elements tree
;;
