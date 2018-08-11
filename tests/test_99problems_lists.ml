open OUnit;;
open Lists;;

let test_last_empty_list_return_none = 
  last []
  |> assert_equal None 
;;

let test_last_non_empty_list_return_last_element = 
  last [ "a" ; "b" ; "c" ; "d" ]
  |> assert_equal (Some "d") 
;;

let test_last_two_empty_list_return_none = 
  last_two []
  |> assert_equal None 
;;

let test_last_two_one_element_list_return_none = 
  last_two ["a"]
  |> assert_equal None 
;;

let test_last_two_4_element_list_return_last_two = 
  last_two [ "a" ; "b" ; "c" ; "d" ]
  |> assert_equal (Some ("c", "d")) 
;;

let test_nth_empty_list_and_zero_index_return_none = 
  nth 0 []
  |> assert_equal None 

let test_nth_empty_list_and_3_index_return_none = 
  nth 3 []
  |> assert_equal None 
;;

let test_nth_5_element_list_and_3_index_return_last_element = 
  nth 3 [ "a" ; "b"; "c"; "d"; "e" ]
  |> assert_equal (Some "c") 
;;

let test_reverse_empty_list_return_empty_list = 
  reverse []
  |> assert_equal []
;;

let test_reverse =
  reverse [1; 2; 3]
  |> assert_equal [3; 2; 1]
;;

let test_is_polindrome_empty_list_return_true = 
  is_polindrome []
  |> assert_bool "False returned"
;;

let test_is_polindrome = 
  is_polindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]
  |> assert_bool "False returned"
;;

let test_is_polindome_false = 
  not (is_polindrome [ "a" ; "b" ])
  |> assert_bool "True returned"
;;

let test_compress = 
  compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> assert_equal ["a"; "b"; "c"; "a"; "d"; "e"]
;;

let test_compress_empty_list_return_empty_list = 
  compress []
  |> assert_equal []
;;

let test_split_when = 
  split_when (fun x -> x = 2) [2; 1; 2]
  |> assert_equal ([2], [1; 2])
;;

let test_split_when_condition_not_satisfied_return_empty_left = 
  split_when (fun x -> false) [1; 2; 3; 4]
  |> assert_equal ([], [1; 2; 3; 4])
;;

let test_split_when_empty_list_return_empty_list = 
  split_when (fun _ -> true) []
  |> assert_equal ([], [])
;;

let test_pack = 
  pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  |> assert_equal [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; 
                   ["a"; "a"]; ["d"; "d"];["e"; "e"; "e"; "e"]]
;;

let test_pack_empty_list_return_empty_list = 
  pack []
  |> assert_equal []
;;

let test_encode = 
  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> assert_equal [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
;;

let test_flatten = 
  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
  |> assert_equal ["a"; "b"; "c"; "d"; "e"]
;;

let test_flatten_empty_list_return_empty_list = 
  flatten []
  |> assert_equal []
;;