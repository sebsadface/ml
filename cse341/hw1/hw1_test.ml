#use "hw1.ml"

let date1 = (9, 4, 2023)
let date2 = (9, 4, 2023)
let date3 = (8, 9, 2023)
let date4 = (15, 1, 2021)
let date5 = (14, 1, 2021)
let date6 = (29, 3, 1890)
let date7 = (18, 12, 1998)

let dates1 = [date1; date2; date3; date4; date5; date6; date7]
let dates2 = [(12, 11, 1020); (25, 11, 2032); (11, 11, 1111); (31, 1, 100); (19, 11, 12); (23, 2, 503); (12, 5, 1012)]
let dates3 = [(12, 5, 1020); (25, 4, 2032); (11, 3, 1111); (31, 1, 100); (19, 2, 12)]
let dates4 = [(1, 1, 1)]
let dates5 = [(2,2,2); (3,3,3)]
let dates6 = [(2,2,2); (3,2,3); (4,2,4)]
let dates7 = [(1,2,3); (4,5,6); (7,8,9); (10, 11, 12)]

(* test 1*)
let test_is_older1 = is_older(date1, date2) = false
let test_is_older2 = is_older(date2, date3) = true
let test_is_older3 = is_older(date3, date4) = false
let test_is_older4 = is_older(date4, date5) = false
let test_is_older5 = is_older(date5, date6) = false
let test_is_older6 = is_older(date6, date7) = true

(* test 2*)
let test_number_in_month1 = number_in_month(dates1, 4) = 2
let test_number_in_month2 = number_in_month(dates1, 12) = 1
let test_number_in_month3 = number_in_month(dates2, 11) = 4
let test_number_in_month4 = number_in_month(dates4, 1) = 1
let test_number_in_month5 = number_in_month(dates3, 9) = 0
let test_number_in_month6 = number_in_month(dates5, 3) = 1

(* test 3*)
let test_number_in_months1 = number_in_months(dates1, [4;9;1;3;12]) = 7
let test_number_in_months2 = number_in_months(dates2, [11;2;3]) = 5
let test_number_in_months3 = number_in_months(dates3, [11;2;3]) = 2
let test_number_in_months4 = number_in_months(dates4, [11;2;3]) = 0
let test_number_in_months5 = number_in_months(dates5, [2]) = 1
let test_number_in_months6 = number_in_months(dates7, [2;5;8;11]) = 4

(* test 4*)
let test_dates_in_month1 = dates_in_month(date1, 4) = [date1; date2]


(* test 5*)

(* test 6*)

(* test 7*)

(* test 8*)

(* test 9*)

(* test 10*)

(* test 11*)

(* test 12*)

(* test 13*)

(* test 14*)