(* NOTE: Uncomment the following line if you want to #use this file in utop
 * (optionally, call it from utop directly): *)
#mod_use "hw3types.ml";;


(* NOTE: to get rid off the red-wiggles in VSCode, first compile the
 * the hw3types module running this 
 * from the command line: 
       ocamlopt hw3types.ml
 *)
open Hw3types

(**** Implement the following functions, remembering the "important note on function bindings" in the assignment write-up ****)

(* #1 *)
let only_lowercase = List.filter (fun x -> Char.lowercase_ascii(x.[0]) = x.[0])

(* #2 *)
let longest_string1 = List.fold_left (fun acc x -> if String.length x > String.length acc then x else acc) ""

(* #3 *)
let longest_string2 = List.fold_left (fun acc x -> if String.length x >= String.length acc then x else acc) ""

(* #4 *)
let longest_string_helper f = List.fold_left (fun acc x -> if f (String.length x) (String.length acc) then x else acc) ""

let longest_string3 = longest_string_helper (>)

let longest_string4 = longest_string_helper (>=)
  
(* #5 *)
let longest_lowercase = longest_string1 % only_lowercase 

(* #6 *)
let caps_no_X_string = String.concat "" % String.split_on_char 'X' % String.capitalize_ascii

(* #7 *)
let rec first_answer f xs = 
  match xs with
  | [] -> raise NoAnswer
  | x :: xs' -> 
    match f x with
    | Some v -> v
    | None -> first_answer xs'

(* #8 *)
let all_answers f xs = 
  let rec helper f acc xs =
    match xs with 
    | [] -> Some acc
    | x :: xs' ->
      match f x with
      | None -> None
      | Some lst -> helper f (acc @ lst) xs'
  in
  helper f [] xs

(* #9 *)
let count_wildcards =
  failwith "Need to implement count_wildcards"

let count_wild_and_variable_lengths =
  failwith "Need to implement count_wild_and_variable_lengths"

let count_a_var s = 
  failwith "Need to implement count_a_var"

(* #10 *)
let check_pat pat = 
  failwith "Need to implement check_pat"

(* #11 *)
let rec matches v pat = 
  failwith "Need to implement matches"

(* #12 *)
let first_match v pats = 
  failwith "Need to implement first_match"

(* optional challenge problem  *)

let typecheck_patterns cons pats = 
  failwith "Need to implement typecheck_patterns"
