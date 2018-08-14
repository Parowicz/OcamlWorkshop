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

let test_from_list =
  T.from_list [1l]
  |> assert_equal (T.Node(1l, T.Empty, T.Empty));

  T.from_list [3l; 2l; 5l]
  |> assert_equal (T.Node(3l, T.Node(2l, T.Empty, T.Empty), T.Node(5l, T.Empty, T.Empty)));

  T.from_list [10l; 11l; 12l]
  |> assert_equal (T.Node(10l, T.Empty, T.Node(11l, T.Empty, T.Node(12l, T.Empty, T.Empty))))
;;

let test_from_list_empty_list_return_empty = 
  T.from_list []
  |> assert_equal T.Empty
;;

let test_max = 
  T.max (T.Node(5l, T.Node(1l, T.Node(-2l, T.Empty, T.Empty),
           T.Node(3l, T.Empty, T.Empty)), T.Empty))
  |> assert_equal (Some(5l))
;;

let test_max_empty_tree_return_None = 
  T.max T.Empty 
  |> assert_equal None
;;

let test_max_of_list = 
  T.from_list [1l; 2l; 3l; 10l; 2l; -10l; 15l; 11l]
  |> T.max
  |> assert_equal (Some(15l))
;;

let test_remove_max = 
  T.remove_max (T.Node(2l, T.Empty, T.Node(5l, T.Empty, T.Empty)))
  |> assert_equal (T.Node(2l, T.Empty, T.Empty));
  
  T.remove_max (T.Node(2l, T.Empty, T.Node(3l, T.Empty, T.Node(4l, T.Empty, T.Empty))))
  |> assert_equal (T.Node(2l, T.Empty, T.Node(3l, T.Empty, T.Empty)))
;;

let test_remove_max_with_left_subnode_keep_subnode =
  T.remove_max (T.Node(0l, T.Empty, T.Node(2l, T.Node(1l, T.Empty, T.Empty), T.Empty)))
  |> assert_equal (T.Node(0l, T.Empty, T.Node(1l, T.Empty, T.Empty)))
;;

let test_remove_max_empty_tree_remove_empty_tree = 
  T.remove_max T.Empty
  |> assert_equal T.Empty
;;

let test_min_empty_tree_return_none = 
  T.min T.Empty |> assert_equal None
;;

let test_min_signleton_return_root_value = 
  T.min (T.Node(5l, T.Empty, T.Empty)) |> assert_equal (Some (5l))
;;

let test_min = 
  T.min (T.Node(5l, T.Node(4l, T.Node(2l, T.Empty, T.Empty), 
          T.Node(6l, T.Empty, T.Empty)), T.Empty))
  |> assert_equal (Some(2l))
;;

let test_remove_element_signleton_return_empty_tree =
  T.remove 21l (T.Node(21l, T.Empty, T.Empty))
  |> assert_equal T.Empty
;;

let test_remove_last_subnode = 
  T.remove 5l (T.Node(21l, T.Node(5l, T.Empty, T.Empty), T.Empty))
  |> assert_equal (T.Node(21l, T.Empty, T.Empty));

  T.remove 35l (T.Node(0l, T.Empty, T.Node(35l, T.Empty, T.Empty)))
  |> assert_equal (T.Node(0l, T.Empty, T.Empty))
;;

let test_remove_root_balanced_tree_align_right = 
  T.remove 0l (T.Node(0l, T.Node(-35l, T.Empty, T.Empty), T.Node(35l, T.Empty, T.Empty)))
  |> assert_equal (T.Node(35l, T.Node(-35l, T.Empty, T.Empty), T.Empty))
;;

let test_remove_root_align_minimal = 
  T.remove 0l (T.Node(0l, T.Node(-35l, T.Empty, T.Node(-25l, T.Empty, T.Empty)), 
                T.Node(35l, T.Node(25l, T.Node(15l, T.Empty, T.Empty), T.Empty), T.Empty)))
  |> assert_equal (T.Node(15l, T.Node(-35l, T.Empty, T.Node(-25l, T.Empty, T.Empty)), 
                    T.Node(35l, T.Node(25l, T.Empty, T.Empty), T.Empty)))
;;

let test_remove_pull_right = 
  T.remove 0l (T.Node(0l, T.Empty, T.Node(1l, T.Empty, T.Node(2l, T.Empty, T.Empty))))
  |> assert_equal (T.Node(1l, T.Empty, T.Node(2l, T.Empty, T.Empty)))
;;

let test_remove_not_existing_item_return_same_tree =
  T.remove 100l (T.Node(0l, T.Node(-35l, T.Empty, T.Empty), T.Node(35l, T.Empty, T.Empty)))
  |> assert_equal (T.Node(0l, T.Node(-35l, T.Empty, T.Empty), T.Node(35l, T.Empty, T.Empty)))
;;

let test_size_empty_return_0 = 
  T.size T.Empty 
  |> assert_equal 0
;;

let test_size_signleton_return_1 = 
  T.size (T.Node(5l, T.Empty, T.Empty))
  |> assert_equal 1
;;

let test_size = 
  T.size (T.Node(5l, T.Node(2l, T.Empty, T.Empty), 
            T.Node(6l, T.Empty, T.Node(8l, T.Empty, T.Empty))))
  |> assert_equal 4
;;