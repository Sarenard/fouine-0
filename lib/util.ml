let print_bool = function
  | true  -> print_string "true"
  | false -> print_string "false"

let rec compare_tuple compare_value lst1 lst2 = match (lst1, lst2) with
  | [], [] -> true
  | [], _ -> false
  | _, [] -> false
  | a::xsa, b::xsb -> (compare_value a b) && compare_tuple compare_value xsa xsb
