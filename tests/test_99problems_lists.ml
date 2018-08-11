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

let test_is_palindrome_empty_list_return_true = 
  is_palindrome []
  |> assert_bool "False returned"
;;

let test_is_palindrome = 
  is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]
  |> assert_bool "False returned"
;;

let test_is_palindome_false = 
  not (is_palindrome [ "a" ; "b" ])
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

let test_endoce_empty_list_return_empty_list = 
  encode []
  |> assert_equal []
;;

let test_flatten = 
  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
  |> assert_equal ["a"; "b"; "c"; "d"; "e"]
;;

let test_flatten_empty_list_return_empty_list = 
  flatten []
  |> assert_equal []
;;

let test_encode_rle = 
  encode_rle ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> assert_equal [Multiple (4, "a"); Single "b"; Multiple (2, "c"); Multiple (2, "a"); Single "d";
                   Multiple (4, "e")]
;;

let test_encode_rlt_empty_list_return_empty_list = 
  encode_rle []
  |> assert_equal []
;;

let test_repeat = 
  repeat 5 'R'
  |> assert_equal ['R'; 'R'; 'R'; 'R'; 'R']
;;

let test_repeat_zero_times_return_empty_list = 
  repeat 0 'R'
  |> assert_equal []
;;

let test_repeat_negative_n_raise_error = 
  assert_raises (Failure "Negative number passed as number of repeats") (fun () -> repeat (-1) 'R')
;;

let test_decode_rle = 
  decode_rle [Multiple (4,"a"); Single "b"; Multiple (2,"c"); 
              Multiple (2,"a"); Single "d"; Multiple (4,"e")]
  |> assert_equal ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

let test_decode_rle_reverse_encode_rlt = 
  let lst = [1; 1; 2; 2; 2; 1; 2; 1]
  in let encoded = encode_rle lst
  in decode_rle encoded 
     |> assert_equal lst
;;

let test_duplicate =
  duplicate ["a";"b";"c";"c";"d"]
  |> assert_equal ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
;;

let test_duplicate_empty_list_return_empty_list = 
  duplicate []
  |> assert_equal []
;;

let test_replicate = 
  replicate ["a";"b";"c"] 3
  |> assert_equal ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
;;

let test_replicate_empty_list_return_empty_list = 
  replicate [] 100
  |> assert_equal []
;;

let test_drop = 
  drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
  |> assert_equal ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
;;

let test_drop_index_out_of_range_return_same_list =
  let lst = [1; 2; 3]
  in drop lst 10 |> assert_equal lst
;;

let test_drop_empty_list_return_empty_list = 
  drop [] 1 |> assert_equal []
;;

let test_drop_index_equal_1_return_empty_list = 
  drop [1; 2; 3; 4; 5] 1 |> assert_equal []
;;

let test_drop_index_lesser_or_equal_to_0_raises_error = 
  assert_raises (Failure "Index <= 0") (fun () -> drop [1; 2] 0);
  assert_raises (Failure "Index <= 0") (fun () -> drop [1; 2] (-12))
;;