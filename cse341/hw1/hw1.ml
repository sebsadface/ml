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
    if snd3(date1) = snd3(date2) then fst3(date1) < fst3(date2) else snd3(date1) < snd3(date2)
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
let string_of_date (date : (int * int * int)) = 
  let str_months = ["January"; "February"; "March"; "April"; "May"; "June"; "July"; "August"; "September"; "October"; "November"; "December"] in
    get_nth(str_months, snd3(date)) ^ "-" ^ string_of_int(fst3(date)) ^ "-" ^ string_of_int(thd3(date))

(* 8 *)
let rec number_before_reaching_sum ((sum : int), (ls : int list)) =
  if sum - List.hd ls <= 0 then
    0
  else
    1 + number_before_reaching_sum((sum - List.hd ls), (List.tl ls))

(* 9 *)
let what_month (day : int) = 
  let months = [31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31] in
    1 + number_before_reaching_sum(day, months)

(* 10 *)
let rec month_range ((day1 : int), (day2 : int)) =
  if day1 > day2 then
    []
  else
    what_month(day1) :: month_range(day1 + 1, day2)

(* 11 *)
let rec oldest (dates : (int * int * int) list) =
  if dates = [] then
    None
  else
    let oldest_tl = oldest(List.tl dates) in
      if Option.is_none oldest_tl || is_older (List.hd dates, Option.get oldest_tl) then Some (List.hd dates) else oldest_tl

(* 12 *)
let cumulative_sum (nums : int list) = 
  let rec sum_helper ((lst : int list), (prev_sum : int)) =
    if lst = [] then
      []
    else
      let new_sum = List.hd lst + prev_sum in
       new_sum :: sum_helper (List.tl lst, new_sum)
  in
  sum_helper(nums, 0)

