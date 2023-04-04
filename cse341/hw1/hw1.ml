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
  if thd3(date1) < thd3(date2) then 
    true
  else 
    if thd3(date1) > thd3(date2) then 
      false
    else
      if snd3(date1) < snd3(date2) then
        true
      else 
        if snd3(date1) > snd3(date2) then
          false
        else
          if fst3(date1) < fst3(date2) then 
            true
          else
            false


(* 2 *)
let rec number_in_month ((dates : (int * int * int) list), (month : int)) =
  0

(* continue for 3 and onward here *)
 