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
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ]]
  |> assert_equal ["a"; "b"; "c"; "d"; "e"]
;;

let test_flatten2 = 
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; Many [ One "c"; One "d" ]];
            One "e" ]; Many [ One "c"; Many [ One "c"; Many [ One "c"; One "d" ]]]]
  |> assert_equal ["a"; "b"; "c"; "c"; "d"; "e"; "c"; "c"; "c"; "d"]
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

let test_split = 
  split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
  |> assert_equal (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
;;

let test_split_n_out_of_list_range_return_whole_list_and_empty_list = 
  split ["a";"b";"c";"d"] 5
  |> assert_equal (["a"; "b"; "c"; "d"], [])
;;

let test_slice = 
  slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
  |> assert_equal ["c"; "d"; "e"; "f"; "g"]
;;

let test_slice_end_index_out_of_list_range_return_elements_to_end_of_list = 
  slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 1000
  |> assert_equal ["d";"e";"f";"g";"h";"i";"j"]
;;

let test_slice_start_index_less_than_1_return_elements_from_start_of_list = 
  let lst = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]
  in slice lst 0 1000
     |> assert_equal lst
;;

let test_slice_start_index_greater_than_end_index_return_empty_list = 
    slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 10 2
    |> assert_equal []
;;

let test_rotate = 
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
  |> assert_equal ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
;;

let test_rotate_negative_n_rotates_to_rigth = 
  rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)
  |> assert_equal ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
;;

let test_rotate_index_same_as_list_len_return_same_list = 
  let lst = ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]
  in rotate lst (List.length lst)
     |> assert_equal lst
;;

let test_remove_at = 
  remove_at 1 ["a"; "b"; "c"; "d"]
  |> assert_equal ["a"; "c"; "d"]
;;

let test_remove_at_index_out_of_range_return_same_list = 
  let lst = [1; 2; 3; 4]
  in remove_at 20 lst 
     |> assert_equal lst
;;

let test_remove_at_index_less_than_0_return_same_list = 
  let lst = [1; 2; 3; 4]
  in remove_at (-20) lst 
     |> assert_equal lst
;;

let test_insert_at = 
  insert_at "alfa" 1 ["a";"b";"c";"d"]
  |> assert_equal ["a"; "alfa"; "b"; "c"; "d"]
;;

let test_insert_at_element_out_of_range_append_last_element = 
  insert_at "alfa" 100 ["a";"b";"c";"d"]
  |> assert_equal ["a"; "b"; "c"; "d"; "alfa"]
;;

let test_insert_at_index_lesser_or_equal_to_0_append_first_element = 
  insert_at "alfa" (-10) []
  |> assert_equal ["alfa"]
;;

let test_range_ascending = 
  range 4 9
  |> assert_equal [4; 5; 6; 7; 8; 9]
;;

let test_range_descending = 
  range 9 4
  |> assert_equal [9; 8; 7; 6; 5; 4]
;;

let test_range_same_staring_and_ending_index_return_one_element_list = 
  range 10 10
  |> assert_equal [10]
;;