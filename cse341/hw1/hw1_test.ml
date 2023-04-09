(* Test cases *)

(* Test cases for is_older *)
let test_is_older_1 = assert (is_older (((5, 4, 2021), (5, 4, 2022))) = true)
let test_is_older_2 = assert (is_older (((1, 6, 2021), (5, 4, 2021))) = false)
let test_is_older_3 = assert (is_older (((5, 4, 2022), (5, 4, 2022))) = false)
let test_is_older_4 = assert (is_older (((31, 12, 1999), (1, 1, 2000))) = true)
let test_is_older_5 = assert (is_older (((29, 2, 2020), (1, 3, 2020))) = true)
let test_is_older_6 = assert (is_older (((15, 4, 2021), (15, 4, 2021))) = false)

(* Test cases for number_in_month *)
let dates_1 = [(1, 1, 2021); (15, 4, 2021); (20, 4, 2021); (31, 12, 2020)]
let test_number_in_month_1 = assert (number_in_month (dates_1, 1) = 1)
let test_number_in_month_2 = assert (number_in_month (dates_1, 4) = 2)
let test_number_in_month_3 = assert (number_in_month (dates_1, 12) = 1)
let test_number_in_month_4 = assert (number_in_month (dates_1, 3) = 0)
let test_number_in_month_5 = assert (number_in_month ([], 6) = 0)
let test_number_in_month_6 = assert (number_in_month (dates_1, 11) = 0)

(* Test cases for number_in_months *)
let test_number_in_months_1 = assert (number_in_months (dates_1, [1; 4]) = 3)
let test_number_in_months_2 = assert (number_in_months (dates_1, [1; 2; 3; 4; 5]) = 3)
let test_number_in_months_3 = assert (number_in_months (dates_1, []) = 0)
let test_number_in_months_4 = assert (number_in_months (dates_1, [12]) = 1)
let test_number_in_months_5 = assert (number_in_months ([], [1; 2; 3; 4]) = 0)
let test_number_in_months_6 = assert (number_in_months (dates_1, [10; 11]) = 0)

(* Test cases for dates_in_month *)
let test_dates_in_month_1 = assert (dates_in_month (dates_1, 1) = [(1, 1, 2021)])
let test_dates_in_month_2 = assert (dates_in_month (dates_1, 4) = [(15, 4, 2021); (20, 4, 2021)])
let test_dates_in_month_3 = assert (dates_in_month (dates_1, 12) = [(31,12, 2020)])
let test_dates_in_month_4 = assert (dates_in_month (dates_1, 3) = [])
let test_dates_in_month_5 = assert (dates_in_month ([], 6) = [])
let test_dates_in_month_6 = assert (dates_in_month (dates_1, 11) = [])

(* Test cases for dates_in_months *)
let test_dates_in_months_1 = assert (dates_in_months (dates_1, [1; 4]) = [(1, 1, 2021); (15, 4, 2021); (20, 4, 2021)])
let test_dates_in_months_2 = assert (dates_in_months (dates_1, [1; 2; 3; 4; 5]) = [(1, 1, 2021); (15, 4, 2021); (20, 4, 2021)])
let test_dates_in_months_3 = assert (dates_in_months (dates_1, []) = [])
let test_dates_in_months_4 = assert (dates_in_months (dates_1, [12]) = [(31, 12, 2020)])
let test_dates_in_months_5 = assert (dates_in_months ([], [1; 2; 3; 4]) = [])
let test_dates_in_months_6 = assert (dates_in_months (dates_1, [10; 11]) = [])

(* Test cases for get_nth *)
let str_list = ["a"; "b"; "c"; "d"; "e"]
let test_get_nth_1 = assert (get_nth (str_list, 1) = "a")
let test_get_nth_2 = assert (get_nth (str_list, 3) = "c")
let test_get_nth_3 = assert (get_nth (str_list, 5) = "e")
let test_get_nth_4 = assert (get_nth (str_list, 2) = "b")
let test_get_nth_5 = assert (get_nth (str_list, 4) = "d")
let test_get_nth_6 = assert (get_nth (["only"], 1) = "only")

(* Test cases for string_of_date *)
let test_string_of_date_1 = assert (string_of_date (1, 1, 2021) = "January-1-2021")
let test_string_of_date_2 = assert (string_of_date (15, 4, 2021) = "April-15-2021")
let test_string_of_date_3 = assert (string_of_date (31, 12, 2020) = "December-31-2020")
let test_string_of_date_4 = assert (string_of_date (29, 2, 2020) = "February-29-2020")
let test_string_of_date_5 = assert (string_of_date (30, 6, 2021) = "June-30-2021")
let test_string_of_date_6 = assert (string_of_date (25, 12, 2023) = "December-25-2023")

(* Test cases for number_before_reaching_sum *)
let test_number_before_reaching_sum_1 = assert (number_before_reaching_sum (10, [2; 4; 6; 8]) = 2)
let test_number_before_reaching_sum_2 = assert (number_before_reaching_sum (14, [2; 4; 6; 8]) = 3)
let test_number_before_reaching_sum_3 = assert (number_before_reaching_sum (0,[2; 4; 6; 8]) = 0)
let test_number_before_reaching_sum_4 = assert (number_before_reaching_sum (1, [1; 1; 1; 1; 1]) = 0)
let test_number_before_reaching_sum_5 = assert (number_before_reaching_sum (5, [1; 1; 1; 1; 1]) = 4)
let test_number_before_reaching_sum_6 = assert (number_before_reaching_sum (15, [1; 2; 3; 4; 5]) = 4)

(* Test cases for what_month *)
let test_what_month_1 = assert (what_month 1 = 1)
let test_what_month_2 = assert (what_month 31 = 1)
let test_what_month_3 = assert (what_month 32 = 2)
let test_what_month_4 = assert (what_month 60 = 3)
let test_what_month_5 = assert (what_month 61 = 3)
let test_what_month_6 = assert (what_month 365 = 12)

(* Test cases for month_range *)
let test_month_range_1 = assert (month_range (1, 7) = [1; 1; 1; 1; 1; 1; 1])
let test_month_range_2 = assert (month_range (32, 61) = [2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 2; 3; 3])
let test_month_range_3 = assert (month_range (1, 1) = [1])
let test_month_range_4 = assert (month_range (31, 32) = [1; 2])
let test_month_range_5 = assert (month_range (59, 60) = [2; 3])
let test_month_range_6 = assert (month_range (364, 365) = [12; 12])

(* Test cases for oldest *)
let test_oldest_1 = assert (oldest dates_1 = Some (31, 12, 2020))
let test_oldest_2 = assert (oldest [(1, 1, 2021); (15, 4, 2021); (20, 4, 2021)] = Some (1, 1, 2021))
let test_oldest_3 = assert (oldest [(15, 4, 2021); (20, 4, 2021)] = Some (15, 4, 2021))
let test_oldest_4 = assert (oldest [] = None)
let test_oldest_5 = assert (oldest [(1, 1, 2021); (1, 1, 2021); (1, 1, 2021)] = Some (1, 1, 2021))
let test_oldest_6 = assert (oldest [(31, 12, 2020); (1, 1, 2021); (15, 4, 2021)] = Some (31, 12, 2020))

(* Test cases for cumulative_sum *)
let test_cumulative_sum_1 = assert (cumulative_sum [1; 2; 3; 4; 5] = [1; 3; 6; 10; 15])
let test_cumulative_sum_2 = assert (cumulative_sum [0; 0; 0; 0; 0] = [0; 0; 0; 0; 0])
let test_cumulative_sum_3 = assert (cumulative_sum [2; 2; 2; 2; 2] = [2; 4; 6; 8; 10])
let test_cumulative_sum_4 = assert (cumulative_sum [1; -1; 1; -1; 1] = [1; 0; 1; 0; 1])
let test_cumulative_sum_5 = assert (cumulative_sum [10; -5; 15; -20; 30] = [10; 5; 20; 0; 30])
let test_cumulative_sum_6 = assert (cumulative_sum [] = [])