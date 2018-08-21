(**
  Simple text encoding using Huffman tree.
*)

(** 
  For simplicity reasons node of Huffman tree serve also as frequency table.
*)
type 'a node = 
  Letter of 'a * int (** Single character and his frequency. *)
  |Node of 'a node * 'a node * int (** Combined nodes and sum of they's frequencies. *)
;;

(**
  @return frequency table if form [('a element, occurences of 'a element)].
*)
let frequencies lst = 
  let rec count_duplicates acc c = function
  |[] -> acc, []
  |x :: xs as l -> if c = x then count_duplicates (acc + 1) c xs
                    else acc, l 
  in let rec aux = function
    |[] -> []
    |character :: _ as l -> let ct, rest =  count_duplicates 0 character l
                              in (character, ct) :: aux rest
  in List.sort compare lst
     |> List.rev 
     |> aux 
     |> List.map (fun (ch, ct) -> Letter(ch, ct))
;;

(**
  Insert node before the first element in frequency table which frequency 
  is greater than the argument.

  @return assuming sorted input, sorted frequency table which contains given node. 
*)
let rec insert_node ele = function
  |[] -> [ele]
  |(Letter(_, c) as x) :: xs | (Node(_, _, c) as x) :: xs 
    -> match ele with 
        Letter(_, ct) | Node(_, _, ct) 
          -> if ct < c then ele :: x :: xs 
              else x :: insert_node ele xs

(**
  @return node with (left_node, right_node, sum of both nodes frequencies).
*)
let combine left_node right_node = 
  match left_node with 
    Letter(_, ct) | Node(_, _, ct) as l 
      -> match right_node with 
          Letter(_, ct2) | Node(_, _, ct2) as r -> Node(l, r, ct + ct2)

(**
  Fold frequency table into single tree.

  @return Huffman tree.
*)
let rec fold_table = function
  |[] -> failwith "Empty Frequency table"
  |[x] -> x
  |x1 :: x2 :: xs -> insert_node (combine x1 x2) xs |> fold_table
;;

(**
  Transform string into char list.
*)
let to_list str =
  let rec exp i acc =
    if i < 0 then acc 
    else exp (i - 1) (str.[i] :: acc) 
  in exp (String.length str - 1) []
;;

(**
  Build Huffman tree from given string.
*)
let build_tree str = to_list str |> frequencies |> fold_table
;;

(**
  Extract binary codes from Huffman tree.
    
  @return efficient encoding for each character in given tree 
    in form [(character, [bits])].
*)
let encode_tree tree = 
  let rec aux acc = function
    |Letter(ch, _)-> [ch, List.rev acc]
    |Node(l, r, _) -> aux (0 :: acc) l @ aux (1 :: acc) r
  in aux [] tree

(**
  Calculate Huffman coding for given string.
  @return efficient encoding for each character in given string 
    in form [(character, [bits])].
*)
let code_table str = build_tree str |> encode_tree
;;