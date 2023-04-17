(* CSE 341, Homework 2 Tests *)
open Hw2
open Hw2challenge
open Json

(* This file provides a list of basic tests for your homework.
 * You will surely want to add more! These tests do not guarantee that your code
 * is correct or will pass autograder tests. 
 * Notice that currently calling any of the functions on hw2.ml will fail,
 * as such, all test functions are commented by default. As you
 * work on implementing the homework functions:
 *   1. Implement the changes on hw2.ml
 *   2. Run `dune build` to make sure everything compiles
 *   3. Uncomment the corresponding test
 *   4. Add more test scenarios
 *   5. Run `dune test` to build and run your tests.
 * If working correctly, `dune test` will complete with no error messages
 *)

(* We leave the first test uncommented to get you started. Until make_silly_json
 * gets implemented, calling `dune test` or `#use "hw2test.ml"` from dune utop 
 * will print a Failure message: *)
 let test1 = 
  make_silly_json 2 
  = 
  Array
    [Object [("n", Num 2.); ("b", True)]; 
     Object [("n", Num 1.); ("b", True)]]
  let test1_1 = make_silly_json 0 = Array []
  let test1_2 = make_silly_json 1 = Array [Object [("n", Num 1.); ("b", True)]]

let test2 = concat_with (";", ["1"; "2"]) = "1;2" 
let test2_1 = concat_with (";", []) = ""
let test2_2 = concat_with (";", ["1"]) = "1"
let test2_3 = concat_with (";", ["1"; "2"; "3"]) = "1;2;3"
let test2_4 = concat_with (";", ["1"; "2"; "3"; "4"]) = "1;2;3;4"

let test3 = quote_string "hello" = "\"hello\""
let test3_1 = quote_string "hello world" = "\"hello world\""
let test3_2 = quote_string "hello world!" = "\"hello world!\""
let test3_3 = quote_string "hello world!!" = "\"hello world!!\""
let test3_4 = quote_string "hello world!!!" = "\"hello world!!!\""

let test4 = string_of_json json_obj = "{\"foo\" : 3.14159, \"bar\" : [1, \"world\", null], \"ok\" : true}"
let test4_1 = string_of_json json_array = "[1, \"world\", null]"
let test4_2 = string_of_json json_hello= "\"hello\""
let test4_3 = string_of_json json_false = "false"

let test5 = take (2, [4; 5; 6; 7]) = [4; 5]
let test5_1 = take (0, [4; 5; 6; 7]) = []
let test5_2 = take (1, [4; 5; 6; 7]) = [4]
let test5_3 = take (2, [4; 5; 6; 7]) = [4; 5]
let test5_4 = take (3, [4; 5; 6; 7]) = [4; 5; 6]

let test6 = firsts [(1,2); (3,4)] = [1; 3]
let test6_1 = firsts [(1,2); (3,4); (5,6)] = [1; 3; 5]
let test6_2 = firsts [(1,2); (3,4); (5,6); (7,8)] = [1; 3; 5; 7]
let test6_3 = firsts [(1,2); (3,4); (5,6); (7,8); (9,10)] = [1; 3; 5; 7; 9]
let test6_4 = firsts [(1,2); (3,4); (5,6); (7,8); (9,10); (11,12)] = [1; 3; 5; 7; 9; 11]

(** don't forget to write a comment for problem 7 **)

let test8 = assoc ("foo", [("bar",17);("foo",19)]) = Some 19
let test8_1 = assoc ("foo", [("bar",17);("foo",19);("foo",20)]) = Some 19
let test8_2 = assoc ("foo", [("bar",17);("foo",19);("foo",20);("foo",21)]) = Some 19
let test8_3 = assoc ("foo", [("bar",17);("foo",19);("foo",20);("foo",21);("foo",22)]) = Some 19
let test8_4 = assoc ("foo", [("bar",17);("foo",19);("foo",20);("foo",21);("foo",22);("foo",23)]) = Some 19

let test9 = dot (json_obj, "ok") = Some True
let test9_1 = dot (json_obj, "foo") = Some (Num 3.14159)
let test9_2 = dot (json_obj, "bar") = Some (Array [Num 1.; String "world"; Null])
let test9_3 = dot (json_obj, "bar") = Some (Array [Num 1.; String "world"; Null])
let test9_4 = dot (json_obj, "bar") = Some (Array [Num 1.; String "world"; Null])


let test10 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"]) = Some (String "gotcha")
let test10_1 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"; "h"]) = None
let test10_2 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"; "h"; "i"]) = None
let test10_3 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"; "h"; "i"; "j"]) = None
let test10_4 = dots (Object [("f", Object [("g", String "gotcha")])], ["f"; "g"; "h"; "i"; "j"; "k"]) = None

let test11 = one_fields json_obj = List.rev ["foo";"bar";"ok"]
let test11_1 = one_fields json_array = []
let test11_2 = one_fields json_hello = []
let test11_3 = one_fields json_false = []


let test12 = not (no_repeats ["foo";"bar";"foo"])
let test12_1 = no_repeats ["foo";"bar";"baz"]
let test12_2 = not (no_repeats ["foo";"bar";"baz";"foo"])
let test12_3 = not (no_repeats ["foo";"bar";"baz";"foo";"bar"])
let test12_4 = not (no_repeats ["foo";"bar";"baz";"foo";"bar";"baz"])


let nest = Array [Object [];
                  Object[("a",True);
                         ("b",Object[("foo",True);
                                     ("foo",True)]);
                         ("c",True)];
                  Object []]
let test13 = not (recursive_no_field_repeats nest)
let test13_1 = recursive_no_field_repeats json_obj
let test13_2 = recursive_no_field_repeats json_array
let test13_3 = recursive_no_field_repeats json_hello
let test13_4 = recursive_no_field_repeats json_false

(* Any order is allowed by the specification, so it's ok to fail this test because of a different order. 
   You can edit this test to match your implementation's order. *)
let test14a = count_occurrences (["a"; "a"; "b"], (Failure "")) = [("b",1);("a",2)]


(* test to see that an exception is thrown when the input list is not sorted *)
let test14b = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
              with Failure _ -> true
let test14b_1 = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
                with Failure _ -> true
let test14b_2 = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
                with Failure _ -> true
let test14b_3 = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
                with Failure _ -> true
let test14b_4 = try count_occurrences (["b"; "a"; "b"], (Failure "")) = []
                with Failure _ -> true

let test15 = string_values_for_access_path (["x"; "y"], [Object [("a", True);("x", Object [("y", String "foo")])];
                                                             Object [("x", Object [("y", String "bar")]); ("b", True)]])
            = ["foo";"bar"]
let test15_3 = string_values_for_access_path (["x"; "y"], [Object [("a", True);("x", Object [("y", String "foo")])];
                                                             Object [("x", Object [("y", String "bar")]); ("b", True)]])
            = ["foo";"bar"]
let test15_4 = string_values_for_access_path (["x"; "y"], [Object [("a", True);("x", Object [("y", String "foo")])];
                                                             Object [("x", Object [("y", String "bar")]); ("b", True)]])
            = ["foo";"bar"]

let test16 = filter_access_path_value (["x"; "y"], "foo",
                                           [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                                            Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                                            Object [("x", String "a")];
                                            Object []])
             = 
             [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
              Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]

let test16_3 = filter_access_path_value (["x"; "y"], "foo",
                                           [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                                            Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                                            Object [("x", String "a")];
                                            Object []])
              =
              [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
               Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]
let test16_4 = filter_access_path_value (["x"; "y"], "foo",
                                           [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
                                            Object [("x", Object [("y", String "foo")]); ("z", String "baz")];
                                            Object [("x", String "a")];
                                            Object []])
              =
              [Object [("x", Object [("y", String "foo")]); ("z", String "bar")];
               Object [("x", Object [("y", String "foo")]); ("z", String "baz")]]



let pgascse =
  { latitude = 47.653221;
    longitude = -122.305708 }

let test17 = in_rect (u_district, pgascse)

let json_pgascse = Object [("latitude", Num 47.653221); ("longitude", Num (-122.305708))]

let test18 = point_of_json json_pgascse = Some pgascse

let test19 = filter_access_path_in_rect (["x"; "y"], u_district, [Object [("x", Object [("y", json_pgascse)])]; Object []])
             = [Object [("x", Object [("y", json_pgascse)])]]


(* Challenge problems *)

let testC1 = consume_string_literal (char_list_of_string "\"foo\" : true") = ("foo", [' '; ':'; ' '; 't'; 'r'; 'u'; 'e'])

let testC2 = consume_keyword (char_list_of_string "false foo") = (FalseTok, [' '; 'f'; 'o'; 'o'])

let testC3 = tokenize_char_list (char_list_of_string "{ \"foo\" : 3.14, \"bar\" : [true, false] }")
             = [LBrace; StringLit "foo"; Colon; NumLit "3.14"; Comma; StringLit "bar";
                Colon; LBracket; TrueTok; Comma; FalseTok; RBracket; RBrace]

let testC5 = parse_string [StringLit "foo"; FalseTok] = ("foo", [FalseTok])

let testC6 = expect (Colon, [Colon; FalseTok]) = [FalseTok]

(** let testC10 = parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
              = (Object [("foo", Null); ("bar", Array [True; False])], []) **)

