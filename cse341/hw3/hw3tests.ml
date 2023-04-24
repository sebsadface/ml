#use "hw3.ml"

let test1_only_lowercase = only_lowercase ["Hello"; "world"; "OCaml"; "functional"] = ["world"; "functional"]
let test2_only_lowercase = only_lowercase ["Apple"; "Banana"; "Cherry"] = []
let test3_only_lowercase = only_lowercase ["apple"; "banana"; "cherry"] = ["apple"; "banana"; "cherry"]


let test1_longest_string1 = longest_string1 ["apple"; "banana"; "cherry"] = "banana"
let test2_longest_string1 = longest_string1 ["apple"; "peach"; "cherry"] = "cherry"
let test3_longest_string1 = longest_string1 [] = ""


let test1_longest_string2 = longest_string2 ["apple"; "banana"; "cherry"] = "cherry"
let test2_longest_string2 = longest_string2 ["apple"; "peach"; "cherry"] = "cherry"
let test3_longest_string2 = longest_string2 [] = ""


let test1_longest_string3 = longest_string3 ["apple"; "banana"; "cherry"] = "banana"
let test2_longest_string3 = longest_string3 ["apple"; "peach"; "cherry"] = "cherry"

let test1_longest_string4 = longest_string4 ["apple"; "banana"; "cherry"] = "cherry"
let test2_longest_string4 = longest_string4 ["apple"; "peach"; "cherry"] = "cherry"


let test1_longest_lowercase = longest_lowercase ["Apple"; "Banana"; "cherry"; "Peach"] = "cherry"
let test2_longest_lowercase = longest_lowercase ["apple"; "banana"; "Cherry"] = "banana"
let test3_longest_lowercase = longest_lowercase ["Apple"; "Banana"; "Cherry"] = ""


let test1_caps_no_X_string = caps_no_X_string "aBxXXxDdx" = "ABDD"
let test2_caps_no_X_string = caps_no_X_string "ocamlXprogramming" = "OCAMLPROGRAMMING"
let test3_caps_no_X_string = caps_no_X_string "xX" = ""


let test1_first_answer = first_answer (fun x -> if x > 10 then Some x else None) [1; 5; 15; 7] = 15
let test2_first_answer = first_answer (fun x -> if x < 0 then Some x else None) [1; 5; -3; 7] = -3
let test3_first_answer = try first_answer (fun x -> None) [1; 2; 3; 4] with NoAnswer -> true | _ -> false


let test1_all_answers = all_answers (fun x -> if x > 0 then Some [x] else None) [1; 2; 3; 4] = Some [1; 2; 3; 4]
let test2_all_answers = all_answers (fun x -> if x > 0 then Some [x] else None) [1; 2; -1; 4] = None
let test3_all_answers = all_answers (fun x -> if x > 0 then Some [x] else None) [] = Some []

let pattern_example = TupleP [WildcardP; VariableP "x"; ConstructorP("C", WildcardP); TupleP [VariableP "y"; VariableP "x"]]

let test1_count_wildcards = count_wildcards pattern_example = 2
let test2_count_wildcards = count_wildcards (TupleP []) = 0
let test3_count_wildcards = count_wildcards (ConstructorP("C", WildcardP)) = 1

let test1_count_wild_and_variable_lengths = count_wild_and_variable_lengths pattern_example = 5
let test2_count_wild_and_variable_lengths = count_wild_and_variable_lengths (TupleP []) = 0
let test3_count_wild_and_variable_lengths = count_wild_and_variable_lengths (ConstructorP("C", VariableP "abc")) = 3

let test1_count_a_var = count_a_var "x" pattern_example = 2
let test2_count_a_var = count_a_var "y" pattern_example = 1
let test3_count_a_var = count_a_var "z" pattern_example = 0


let test1_check_pat = check_pat pattern_example = false
let test2_check_pat = check_pat (TupleP [VariableP "x"; VariableP "y"; VariableP "z"]) = true
let test3_check_pat = check_pat (TupleP [VariableP "x"; VariableP "y"; VariableP "x"]) = false


let value_example = Tuple (Unit :: [Constant 42; Constructor ("C", Unit); Tuple [Constant 1; Constant 2]])
let pattern_example2 = TupleP [UnitP; VariableP "a"; ConstructorP("C", WildcardP); TupleP [VariableP "b"; VariableP "c"]]
let pattern_example3 = TupleP [WildcardP; VariableP "x"; ConstructorP("C", WildcardP); TupleP [ConstantP 2; VariableP "x"]]

let test1_matches = matches value_example pattern_example = Some [("x", Constant 42); ("y", Constant 1); ("x", Constant 2)]
let test2_matches = matches value_example pattern_example2 = Some [("a", Constant 42); ("b", Constant 1); ("c", Constant 2)]
let test3_matches = matches (Constant 42) (VariableP "x") = Some [("x", Constant 42)]
let test4_matches = matches value_example pattern_example3 = None

let patterns_example = [pattern_example; pattern_example2]

let test1_first_match = first_match value_example patterns_example = Some [("x", Constant 42); ("y", Constant 1); ("x", Constant 2)]
let test2_first_match = first_match (Constant 42) [WildcardP; VariableP "x"] = Some []
let test3_first_match = first_match (Constant 42) [VariableP "x"; WildcardP] = Some [("x", Constant 42)]
