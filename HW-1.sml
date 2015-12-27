
(* is_older function checks if the 1st date is older than the 2nd one*)

fun is_older(d1 : int * int * int,d2 : int * int * int) = 
if #1 d1 > #1 d2
 then false
else if #1 d1 < #1 d2
 then true
else
 if #2 d1 > #2 d2
  then false
 else if #2 d1 < #2 d2
  then true
 else
  if #3 d1 > #3 d2  
   then false
  else if #3 d1 < #3 d2
   then true
  else false

(* number_in_month - Checks if the month matches with any of the dates*)
fun number_in_month(dates: (int * int * int) list,month:int) = 
let
 fun match(date:int * int * int) =
    if #2 date = month
    then 1
    else 0
in
   if null dates
   then 0
   else if null (tl dates)
   then match(hd dates)
   else match(hd dates) + number_in_month(tl dates,month)
end

(* number_in_months - checks if any of the months(unique) match with any dates *)
fun number_in_months(dates:(int * int *int) list,months:int list) = 
if null months
then 0
else
number_in_month(dates,hd months) + number_in_months(dates,tl months)
  
(* dates_in_month - returns a list of dates in the month *)
fun dates_in_month(dates:(int * int *int)list,month:int) = 
let
 fun add_month(date:(int * int *int)) = 
    #2 date = month    		     
in
if null (tl dates)
then 
  if add_month(hd dates)
  then hd dates::[]
  else []
else 
  if add_month(hd dates)
  then hd dates::dates_in_month(tl dates,month)
  else dates_in_month(tl dates,month) 
end

(* dates_in_months *)
fun dates_in_months(dates:(int * int * int) list,months:int list) = 
if null months
then []
else
 let val tmp_list = dates_in_month(dates, hd months)
 in 
 if null tmp_list  (*::dates_in_months(dates,tl months) *)
 then dates_in_months(dates,tl months)
 else tmp_list@dates_in_months(dates,tl months)
 end
 
(* get_nth returns the nth string in the list, list starts from 1*)
fun get_nth (strings: string list, num:int) = 
let 
  fun check(xs:string list,count:int) =  
  if count = num
  then hd xs  
  else check(tl xs,count+1)
in
  check(strings,1)
end

(* date_to_string converts a date object to string format *)
fun date_to_string(date: int * int * int) = 
let val months = ["January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December" ];
in
 get_nth(months,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date) 
end

(* number_before_reaching_sum int * int list *)
fun number_before_reaching_sum(sum:int,nums:int list) = 
let fun sum_check(tmp_sum:int,n:int,xs:int list) = 
    if tmp_sum >= sum orelse null xs
    then n
    else sum_check(tmp_sum+ hd xs,n+1,tl xs)
in sum_check(0,0,nums)
end

