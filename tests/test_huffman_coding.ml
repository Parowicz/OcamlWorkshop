open OUnit;;
open HuffmanCoding;;

let test_frequencies_empty_list_return_empty_list = 
  frequencies []
  |> assert_equal []
;;

let test_frequencies_list_of_same_elements_return_element_and_list_length = 
  let lst = ['a'; 'a'; 'a'; 'a']
  in match frequencies lst with
     |[Letter(element, count)] when element = 'a' -> assert_equal count 4
     |_ -> assert_failure "Haven't returned Letter('a', 4)"
;;

let test_frequencies =
  let lst = frequencies ['a'; 'c'; 'a'; 'c'; 'v']
  in 
    List.mem (Letter('a', 2)) lst |> assert_bool "Haven't found Letter('a', 2)";
    List.mem (Letter('c', 2)) lst |> assert_bool "Haven't found Letter('c', 2)";
    List.mem (Letter('v', 1)) lst |> assert_bool "Haven't found Letter('v', 1)";
    List.length lst |> assert_equal 3
;;

let test_insert_node_empty_list_return_singleton_with_that_node = 
  insert_node (Letter('a', 2)) []
  |> assert_equal [Letter('a', 2)]
;;

let test_insert_node = 
  insert_node (Letter('a', 3)) [Letter('b', 2); Letter('c', 4)]
  |> assert_equal [Letter('b', 2); Letter('a', 3); Letter('c', 4)]
;;

let test_insert_node_element_greater_in_list_return_list_with_that_element_on_end = 
  insert_node (Letter('a', 100)) [Letter('b', 2); Letter('c', 4)]
  |> assert_equal [Letter('b', 2); Letter('c', 4); Letter('a', 100)]
;;

let test_combine_two_letters = 
  combine (Letter('a', 5)) (Letter('b', 8))
  |> assert_equal (Node(Letter('a', 5), Letter('b', 8), 13))
;;

let test_combine_two_nodes = 
  let node1 = Node(Letter('a', 5), Letter('b', 8), 13)
  in let node2 = Node(Letter('g', 2), Letter('f', 8), 10)
  in combine node1 node2
     |> assert_equal (Node(node1, node2, 23))
;;

let test_combine_node_and_letter = 
  let node1 = Node(Letter('a', 5), Letter('b', 8), 13)
  in combine (Letter('z', 2)) node1
     |> assert_equal (Node((Letter('z', 2)), node1, 15))
;;

let test_fold_table_empty_list_raise_error = 
  assert_raises (Failure("Empty Frequency table")) (fun _ -> fold_table []) 
;;

let test_to_list_empty_string_return_empty_list = 
  to_list ""
  |> assert_equal []
;;

let test_to_list = 
  to_list "asd"
  |> assert_equal ['a'; 's'; 'd']
;;