open OUnit;;
open LexicalTree;;

let test_contains_empty_tree_doesnt_contains_anything =
  not (StringTree.contains "somthing" StringTree.empty)
  |> assert_bool "Somthing found in empty tree"
;; 

let test_contains_signleton =
  StringTree.contains "s" [StringTree.Letter('s', true, [])]
  |> assert_bool "Letter not found"
;;

let test_contains =
  StringTree.contains "sa" [StringTree.Letter('s', false, [StringTree.Letter('a', true, [])])]
  |> assert_bool "Word havent found";

  StringTree.contains "sa" [StringTree.Letter('s', false, 
                              [StringTree.Letter('b', true, []); StringTree.Letter('a', true, [])])]
  |> assert_bool "Word havent found";
;;

let test_contains_respect_word_ending = 
  not (StringTree.contains "s" [StringTree.Letter('s', false, [StringTree.Letter('a', true, [])])])
  |> assert_bool "Accepted word without end flag";

  StringTree.contains "s" [StringTree.Letter('s', true, [StringTree.Letter('a', true, [])])]
  |> assert_bool "Rejected word with end flag"
;;

let test_contains_empty_word_return_false = 
  not (StringTree.contains "" [StringTree.Letter('s', false, [StringTree.Letter('a', true, [])])])
  |> assert_bool "Tree cannot hold empty word"
;;


let test_insert_contains = 
  let words = ["Victor"; "Van"; "Vector"; "Sam"; "L"; 
                "Luiz"; "Izzak"; "Sentry"; "T"] in
  let tree = List.fold_left (fun acc a -> StringTree.insert a acc) StringTree.empty words
  in List.iter (fun w -> assert_bool (Printf.sprintf "Havent found: %s" w)
                                     (StringTree.contains w tree)) words;
  ["Victors"; ""; "Same"; "Z"; "Izzac"; "Ruiz"; "Centry"]
  |>  List.iter (fun w -> assert_bool (Printf.sprintf "Found element: %s" w)
                                      (not (StringTree.contains w tree)))
;;

