(**
  Lexical tree is data structure well suited to work as lexical words set.

  @author Artur P.
  @version 0.0
*)

(**
  Each Word must expose indexer to single element ane length function.
  Assuming standard string implementation, both should be accessible in constant time.
  Of course we can find list based implementations of string (for example Haskell), in
  this case that module can cause performance problems.
*)
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

  (**
    @return empty list
  *)
  let empty = []

  (**
    Pack word into a stream. ToDo: It could be nice to expose this in Word signature.
    
    @return stream iterator for word letters.
  *)
  let letter_stream word =
    let len = W.length word in
    let safe_get i = if i >= len then None else Some(W.element_at i word) in
    Stream.from safe_get
  
  (**
    @return true if given tree contains given word. 
      If word is empty, then false will be returned.
  *)
  let contains word tree =
    let stream = letter_stream word in
    let rec aux ch lst = 
      match lst with
      |[] -> false
      |Letter(character, ends, subtree) :: _ when character = ch 
        -> (try aux (Stream.next stream) subtree with Stream.Failure -> ends)
      |_ :: xs -> aux ch xs
    in try aux (Stream.next stream) tree with Stream.Failure -> false
      
  (**
      @param word to insert 
      @return new tree with inserted word. If word was empty or tree already contains given word,
       then same tree will be returned.
  *)
  let insert word tree =
    let stream = letter_stream word in
    let rec aux ch lst = 
      match lst with
      |[] -> 
        (try [Letter(ch, false, aux (Stream.next stream) [])]
          with Stream.Failure -> [Letter(ch, true, [])])
      |Letter(character, ends, subtree) :: xs when character = ch
        -> (try Letter(ch, ends, aux (Stream.next stream) subtree) 
              with Stream.Failure -> Letter(ch, true, subtree) ) :: xs
      |x :: xs -> x :: aux ch xs 
    in try aux (Stream.next stream) tree with Stream.Failure -> tree
end

(**
  String based implementation of Word. 
*)
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