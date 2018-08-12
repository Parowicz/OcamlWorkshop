module type Word = sig
  type letter
  type t

  val empty: t
  val from_letter: letter -> t
  val element_at: int -> t -> letter
  val length: t -> int
  val concat: t -> t -> t
end

module type S = sig
  type word
  type t

  val empty: t
  val insert: word -> t -> t
  val contains: word -> t -> bool
end

module Make (W: Word) = struct
  type word = W.t
  type node = Letter of W.letter * bool * t
  and t = node list

  let empty = []

  let contains w tree =
    let len = W.length w in 
    let rec aux i lst = 
      let ch = W.element_at i w in 
      let is_end = i = len - 1 in
      match lst with
      |[] -> false
      |Letter(character, ends, subtree) :: _ when character = ch 
        -> if is_end then ends else aux (i + 1) subtree
      |_ :: xs -> aux i xs
    in aux 0 tree
      
  let insert w tree =
    let len = W.length w in 
    let rec aux i lst = 
      let ch = W.element_at i w in 
      let is_end = i = len - 1 in
      match lst with
      |[] -> [Letter(ch, is_end, if is_end then [] else aux (i + 1) [])]
      |Letter(character, _, subtree) :: xs when character = ch
        -> Letter(ch, is_end, if is_end then subtree else aux (i + 1) subtree) :: xs
      |x :: xs -> x :: aux i xs 
    in aux 0 tree
end

module StringWord = struct
  type letter = char
  type t = string

  let empty = ""
  let from_letter = String.make 1 
  let element_at n w = String.get w n 
  let length = String.length
  let concat w1 w2 = String.concat w1 [w2]
end

module StringTree = Make(StringWord);;