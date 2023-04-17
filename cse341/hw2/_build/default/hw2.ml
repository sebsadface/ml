(* CSE 341, HW2 Provided Code *)
(* This is from file json.ml in this directory. json.ml
 * contains the main datatype definition we will use throughout the
 * assignment. You will want to look over this file before starting. *)
include Json

(* These come from the parsed_*_bus.ml.
   Each file binds one variable: small_bus_positions (10 reports),
   medium_bus_positions (100 reports), and complete_bus_positions (~1000 reports),
   respectively with the data that you will need to implement
   your homework.
*)
open Json_structures.Parsed_complete_bus
open Json_structures.Parsed_medium_bus
open Json_structures.Parsed_small_bus

(* provided helper function that deduplicates a list *)
let dedup xs = List.sort_uniq compare xs

(* provided helper function that sorts a given list *)
let sort xs = List.sort compare xs

(* provided helper function to convert a float to a string *)
(* OCaml's string_of_float is not quite RFC compliant due to its tendency
   to output whole numbers with trailing decimal points without a zero.
   But, printf does the job how we want. *)
let json_string_of_float f =
  Printf.sprintf "%g" f
  
(* 1 *)
let make_silly_json i =
  let rec helper n = 
    match n with
    | 0  -> []
    | _ -> Object[("n", Num (float_of_int n)); ("b", True)] :: helper(n - 1)
  in
  Array (helper i)

(* 2 *)
let rec concat_with (sep, ss) =
  match ss with
  | [] -> ""
  | a :: [] -> a
  | a :: ss' -> a ^ sep ^ concat_with(sep, ss')

(* 3 *)
let quote_string s = "\"" ^ s ^ "\""

(* 4 *)
let rec string_of_json j =
  let rec array_helper js = 
    match js with
    | [] -> []
    | j :: js' -> string_of_json j :: array_helper js'
  in
  let rec object_helper jo =
    match jo with
    | [] -> []
    | j :: jo' -> 
      match j with (name, content) -> (quote_string(name) ^ " : " ^ string_of_json(content)) :: object_helper jo'
  in
  match j with
  | Num f -> json_string_of_float f
  | String s -> quote_string s
  | False -> "false"
  | True -> "true"
  | Null -> "null"
  | Array js -> "[" ^ concat_with(", ", array_helper js) ^ "]"
  | Object jo -> "{" ^ concat_with(", ", object_helper jo) ^ "}"

(* 5 *)
let rec take (n,xs) = 
  match xs with 
  | [] -> []
  | s :: xs' ->
    match n with 0 -> [] | _ -> s :: take(n - 1, xs')

(* 6 *)
let rec firsts xs = 
  match xs with 
  | [] -> []
  | s :: xs' -> match s with (a , b) -> a :: firsts(xs')

(* 7 *)
(* The two expressions always evaluate to the same value because
   they both take the first n elements of the list xs and then extract
   the first element of each pair. The order of these operations does 
   not affect the result beacause both firsts and take don't change the
   order of the list. 
   
   In terms of evaluation time, firsts(take (n, xs)) might be faster because 
   firsts, in this case, only needs to traverse the first n elements of the 
   list xs, whereas take(n, firsts xs) requires firsts to traverse the entire list 
   to extract the first element of each pair.*)

(* 8 *)
let rec assoc (k, xs) =
  match xs with 
  | [] -> None
  | s :: xs' -> match s with (k1, v1) -> if k1 = k then Some v1 else assoc(k, xs')

(* 9 *)
let dot (j, f) = 
  match j with
  | Object obj -> assoc(f, obj) 
  | _ -> None

(* 10 *)
let rec dots (j, fs) =
  match fs with 
  | [] -> None
  | f :: [] -> dot(j, f)
  | f :: fs' -> 
    match dot(j, f) with 
    | None -> None
    | Some v -> dots(v, fs')  
  
(* 11 *)
let one_fields j =
  let rec loop (obj, acc) = 
    match obj with
    | [] -> acc
    | j :: obj' -> match j with (name, content) -> loop(obj', name :: acc)
  in
  match j with 
  | Object obj -> loop(obj, [])
  | _ -> []

(* 12 *)
let no_repeats xs = List.length xs = 0 || dedup xs = sort xs

(* 13 *)
let rec recursive_no_field_repeats j = 
  let rec arr_helper arr =
    match arr with
    | [] -> true
    | j :: arr' -> recursive_no_field_repeats j && arr_helper arr'
  in
  let rec obj_helper obj = 
    match obj with
    | [] -> true
    | j :: obj' -> match j with (name, content) -> recursive_no_field_repeats(content) && obj_helper(obj')
  in
  match j with
  | Object obj -> no_repeats(one_fields j) && obj_helper obj
  | Array arr -> arr_helper arr
  | _ -> true

(* 14 *)
let count_occurrences (xs, e) =
  let rec loop (xs, e, cur_str, cur_count, acc) =
    match xs with
    | [] -> (cur_str, cur_count) :: acc
    | s :: xs' -> 
      if s < cur_str then
       raise e
      else if cur_str = s then 
        loop(xs', e, cur_str, cur_count + 1, acc)
      else 
        loop(xs', e, s, 1, if cur_count = 0 then acc else (cur_str, cur_count) :: acc)
  in
  loop(xs, e, "", 0, [])

(* 15 *)
let rec string_values_for_access_path (fs, js) = 
  match js with
  | [] -> []
  | j :: js' -> 
    match dots(j, fs) with
    | Some String s -> s :: string_values_for_access_path(fs, js')
    | _ -> string_values_for_access_path(fs, js')

(* 16 *)
let rec filter_access_path_value (fs, v, js) = 
  match js with
  | [] -> []
  | j :: js' -> 
    match dots(j, fs) with
    | Some String s -> if s = v then j :: filter_access_path_value(fs, v, js') else filter_access_path_value(fs, v, js')
    | _ -> filter_access_path_value(fs, v, js')

(* Types for use in problems 17-20. *)
type rect = { min_latitude: float; max_latitude: float;
              min_longitude: float; max_longitude: float }
type point = { latitude: float; longitude: float }

(* 17 *)
let in_rect (r, p) = 
  p.latitude <= r.max_latitude && p.latitude >= r.min_latitude && p.longitude <= r.max_longitude && p.longitude >= r.min_longitude

(* 18 *)
let point_of_json j = 
  match dot(j, "latitude"), dot(j, "longitude") with
  | Some Num la, Some Num lo -> Some {latitude = la; longitude = lo}
  | _ -> None

(* 19 *)
let rec filter_access_path_in_rect (fs, r, js) = 
  match js with 
  | [] -> []
  | j :: js' -> 
    match dots(j, fs) with
    | None -> filter_access_path_in_rect(fs, r, js')
    | Some h -> 
      match point_of_json h with
      | Some p -> if in_rect(r, p) then j :: filter_access_path_in_rect(fs, r, js') else filter_access_path_in_rect(fs, r, js')
      | None -> filter_access_path_in_rect(fs, r, js')

(* 20 *)
(* Both filter_access_path_value and filter_access_path_in_rect are recursive functions that take in a JSON list and filter them 
based on certain conditions. One way to refactor these functions could be to create a higher-order function that takes in a 
filtering function as an argument, and use it to filter the list of JSON objects. Then, we could define filter_access_path_value 
and filter_access_path_in_rect as functions that take in the appropriate filtering function as an argument and pass it to the 
higher-order function. My annoyance level was 10 out of 10 *)

(* For this section, we provide the definition of U district and the functions
 * to calculate a histogram. Use these to create the bindings as requested. 
 * But notice our implementation of histogram uses *your* definition of count_occurrences
 *)
 (* We provide this code commented out because it uses some of your functions 
    that you haven't implemented yet *) 
exception SortIsBroken

(* The definition of the U district for purposes of this assignment :) *)
let u_district =
  { min_latitude  =  47.648637;
    min_longitude = -122.322099;
    max_latitude  =  47.661176;
    max_longitude = -122.301019
  }

(* Creates a histogram for the given list of strings. 
 * Returns a tuple in which the first element is
 * a string, and the second is the number of times that string
 * is found. *)
let histogram xs = 
  let sorted_xs = List.sort (fun a b -> compare a b) xs in
  let counts = count_occurrences (sorted_xs, SortIsBroken) in
  let compare_counts (s1, n1) (s2, n2) =
    if n1 = n2 then compare s1 s2 else compare n1 n2
  in
  List.rev (List.sort compare_counts counts)

let histogram_for_access_path (fs, js) = 
  histogram (string_values_for_access_path (fs,js))

(* notice we use *your* definition of dot *)
let complete_bus_positions_list =
  match (dot (complete_bus_positions, "entity")) with
  | Some (Array xs) -> xs
  | _ -> failwith "complete_bus_positions_list"

exception Unimplemented
let route_histogram     = histogram_for_access_path(["vehicle"; "trip"; "route_num"], complete_bus_positions_list)
let top_three_routes    = firsts(take(3, route_histogram))
let buses_in_ud         = filter_access_path_in_rect(["vehicle"; "position"], u_district, complete_bus_positions_list)
let ud_route_histogram  = histogram_for_access_path(["vehicle"; "trip"; "route_num"], buses_in_ud)
let top_three_ud_routes = firsts(take(3, ud_route_histogram))
let all_fourty_fours    = filter_access_path_value(["vehicle"; "trip"; "route_num"], "44", complete_bus_positions_list)
