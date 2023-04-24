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
let caps_no_X_string = String.concat "" % String.split_on_char 'X' % String.uppercase_ascii

(* #7 *)
let rec first_answer f xs = 
  match xs with
  | [] -> raise NoAnswer
  | x :: xs' -> 
    match f x with
    | Some v -> v
    | None -> first_answer f xs'

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

(* The function g takes three curried arguments: 
    f1: a function that takes no arguments and returns a value, which is applied when the pattern is a wildcard.
    f2: a function that takes a string argument and returns a value, which is applied when the pattern is a variable.
    p: a value of type pattern, which represents the pattern to be matched against.

  The function g computes a value based on the input pattern p. It does so by recursively matching the pattern with 
  its different constructors (e.g. wildcard, variable, constructor, and tuple) and applying the corresponding 
  functions f1 or f2 depending on the pattern type. For constructor and tuple patterns, the function folds the results
  from the recursive calls.*)

let count_wildcards = g (fun () -> 1 ) (fun _ -> 0)

let count_wild_and_variable_lengths = g (fun () -> 1) (fun p -> String.length p)

let count_a_var s = g (fun () -> 0) (fun p -> if p = s then 1 else 0)

(* #10 *)
let check_pat pat = 
  let rec helper1 p = 
    match p with
    | VariableP s -> [s]
    | ConstructorP (_,p) -> helper1 p
    | TupleP ps -> List.fold_left (fun acc p -> helper1 p @ acc) [] ps
    | _ -> []
  in
  let rec helper2 xs =
    match xs with
    | [] -> true
    | x :: xs' ->  not (List.mem x xs') && helper2 xs' 
  in 
  pat |> helper1 |> helper2

(* #11 *)
let rec matches v pat = 
  match v, pat with
  | _, WildcardP -> Some []
  | _, VariableP s -> Some [(s,v)]
  | Unit, UnitP -> Some []
  | Constant v, ConstantP i -> if v = i then Some [] else None
  | Constructor (s2, v), ConstructorP (s1, p) ->  if s1 = s2 then matches v p else None
  | Tuple vs, TupleP ps -> 
    if List.length vs = List.length ps then 
      all_answers (fun (v,p) -> matches v p) (List.combine vs ps) 
    else 
      None
  | _ -> None

(* #12 *)
let first_match v pats = 
  try Some (first_answer (matches v) pats) with NoAnswer -> None

(* optional challenge problem  *)

let typecheck_patterns cons pats = 
  failwith "Need to implement typecheck_patterns"
