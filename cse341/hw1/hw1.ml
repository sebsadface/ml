(* CSE 341, Homework 1, Provided Code *)

(* You might choose to uncomment these, like the lecture code does *)
(* #utop_prompt_dummy
   let _ = UTop.set_show_box false *)

(* Use these functions to extract parts of a date *)
let fst3 (x,_,_) = x (* gets the first element of a triple *)
let snd3 (_,x,_) = x (* gets the second element of a triple *)
let thd3 (_,_,x) = x (* gets the third element of a triple *)

(**
 * TODO: Complete the 12 function bindings described in the assignment.  For the first (2), 
 * we have given you the correct first line and an incorrect function body.
 *)

 (* 1 *)
let is_older ((date1 : int * int * int), (date2 : int * int * int)) =
  if thd3(date1) = thd3(date2) then
    if snd3(date1) = snd3(date2) then
      fst3(date1) < fst3(date2)
    else
      snd3(date1) < snd3(date2)
  else
    thd3(date1) < thd3(date2)

(* 2 *) 
let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  if dates = [] then
    0
  else
    (if snd3(List.hd dates) = month then 1 else 0) + number_in_month(List.tl dates, month)
(* continue for 3 and onward here *)
 
(* 3 *)
let rec number_in_months ((dates : (int * int * int) list), (months : int list)) = 
  if months = [] then
    0
  else 
    number_in_month(dates, List.hd months) + number_in_months(dates, List.tl months)

(* 4 *)
let rec dates_in_month ((dates : (int * int * int) list), (month : int)) = 
  if dates = [] then
    []
  else
    if snd3(List.hd dates) = month then 
      snd3(List.hd dates) :: dates_in_month(List.tl dates, month) 
    else 
      dates_in_month(List.tl dates, month)

(* 5 *)
let rec dates_in_months ((dates : (int * int * int) list), (months : int list)) =
  if months = [] then
    []
  else 
    dates_in_month(dates, List.hd months) @ dates_in_months(dates, List.tl months)

(* 6 *)
let rec get_nth ((ls : string list), (n : int)) = 
  if n = 1 then
    List.hd ls
else 
  get_nth(List.tl ls, n - 1)

(* 7 *)
let rec string_of_date (date : (int * int * int)) = 
  let str_months = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
    get_nth(str_months, snd3(date)) ^ "-" ^ string_of_int(fst3(date)) ^ "-" ^ string_of_int(thd3(date))