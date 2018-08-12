open BinaryTree;;
open OUnit;;

module T = BinaryTree.Make(Int32);;

let test_append_empty_tree =
  let value = 1l in 
  T.append value T.empty
  |> assert_equal (T.Node(value, T.Empty, T.Empty))
;;

let test_append_greater_than_root_value_return_append_right_node =
  let value = 20l in 
  T.append value (T.Node(10l, T.Empty, T.Empty))
  |> assert_equal (T.Node(10l, T.Empty, T.Node(value, T.Empty, T.Empty)))
;;

let test_append_greater_than_root_value_return_append_right_node =
  let value = 5l in 
  T.append value (T.Node(10l, T.Empty, T.Empty))
  |> assert_equal (T.Node(10l, T.Node(value, T.Empty, T.Empty), T.Empty))
;;

let test_append_nested_tree_and_value_greater_than_all_elements =
  let value = 13l in
  T.append value (T.Node(10l, T.Empty, T.Node(11l, T.Empty, T.Node(12l, T.Empty, T.Empty))))
  |> assert_equal (T.Node(10l, T.Empty, T.Node(11l, T.Empty, 
                                                T.Node(12l, T.Empty, T.Node(value, T.Empty, T.Empty)))))
;;

let test_constains_empty_tree_always_return_false = 
  T.contains 1l T.empty |> assert_equal false 
;;

let test_constains = 
  T.Node(1l, T.Empty, T.Empty)
  |> T.contains 1l
  |> assert_equal true
;;

let test_contains_deep_tree = 
  T.Node(1l, T.Node(0l, T.Empty, T.Empty), 
            T.Node(2l, T.Empty, T.Node(3l, T.Empty, T.Node(10l, T.Empty, T.Empty))))
  |> T.contains 10l
  |> assert_equal true
;;

let test_flatten_empty_return_empty_list =
  T.flatten T.Empty |> assert_equal []
;;

let test_flatten = 
  T.flatten (T.Node(5l, T.Node(1l, T.Node(-2l, T.Empty, T.Empty), 
              T.Node(3l, T.Empty, T.Empty)), T.Empty))
  |> assert_equal [-2l; 1l; 3l; 5l];

  T.flatten (T.Node(0l, T.Node(-6l, T.Empty, T.Node(-3l, T.Empty, T.Empty)), T.Empty))
  |> assert_equal [-6l; -3l; 0l]
;;

let test_sort = 
  T.sort [4l; 3l; 2l; 1l; 1l; 8l; -10l; 3l; 13l]
  |> assert_equal [-10l; 1l; 1l; 2l; 3l; 3l; 4l; 8l; 13l];

  T.sort [-2l; -10l; -5l]
  |> assert_equal [-10l; -5l; -2l]
;;

let test_sort_with_duplicates = 
  T.sort [3l; 3l; 1l; 1l; 4l; 4l]
  |> assert_equal [1l; 1l; 3l; 3l; 4l; 4l] 
;;

let test_sort_empty_list_return_empty_list = 
  T.sort [] |> assert_equal []
;;