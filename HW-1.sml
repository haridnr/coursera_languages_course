
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
    if null xs orelse tmp_sum + hd xs >= sum
    then n
    else sum_check(tmp_sum+ hd xs,n+1,tl xs)
in sum_check(0,0,nums)
end

(* what_month *)
fun what_month(day:int) = 
let val nums = [31,28,31,30,31,30,31,31,30,31,30,31]
in
 number_before_reaching_sum(day,nums)+1
end

(* month_range *)
fun month_range(day1:int,day2:int) = 
let
  val m_day1 = what_month(day1)
  val m_day2 = what_month(day2)
  fun count(from:int,num:int) = 
   if num > day2-day1+1
   then []
   else
    if from=m_day2
    then from::count(from,num+1)
    else from::count(from+1,num+1)
in 
 if day1>day2
 then [] 
 else count(m_day1,1)
end 

(* Returns the oldest date in the range(type OPTION) *)
fun oldest(dates:(int * int *int)list) = 
if null dates
then NONE
else
  let fun check(xs:(int * int *int)list) = 
    if null (tl xs)
    then hd xs
    else 
     let val temp = check(tl xs)
     in
      if is_older(hd xs,temp)
      then hd xs
      else temp
     end
   in
    SOME (check(dates))
   end   	     

(* remove_duplicates (int list)*)
fun remove_duplicates(nums: int list) = 
 let
  fun match(xs:int list,num:int) = 
    if null xs
    then false
    else if hd xs = num
    then true
    else
    match(tl xs,num)

 fun adder(curr:int list,itr:int list) = 
   if null itr
   then curr
   else
    if match(curr,hd itr)
    then adder(curr,tl itr) 
    else adder(hd itr::curr,tl itr)
 in
  adder([],nums)
end

(* number_in_months_challenge *)
fun number_in_months_challenge(dates:(int * int * int) list,months:int list) = 
let
  val unique_months = remove_duplicates(months)
in
 number_in_months(dates,unique_months)
end

(* dates_in_months_challenge *)
fun dates_in_months_challenge(dates:(int * int *int)list,months:int list) = 
let
  val unique_months = remove_duplicates(months)
in
  dates_in_months(dates,unique_months)
end
      
      
