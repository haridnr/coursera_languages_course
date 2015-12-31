(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str,xs) = 
let fun remove(strs,acc) = 
                 case strs of
                 [] => []
                 | x::xs' =>             
                   if same_string(str,x)
                   then acc@xs'
	           else remove(xs',x::acc)
in	 		     
  case remove(xs,[]) of
      [] => NONE
   |  x => SOME x
   
end          

fun get_substitutions1 (lst_lstStrings,str) = 
case lst_lstStrings of 
  [] => [] 
 | x::xs' =>  case all_except_option(str,x) of
              NONE => get_substitutions1(xs',str) 
	    | SOME x => x @ get_substitutions1(xs',str)
  

fun get_substitutions2 (lst_lstStrings,str) = 
let
  fun accumalate(xlst_lstStrings,acc) = 
       case xlst_lstStrings of
         [] => acc
       | x::xs' => case all_except_option(str,x) of
                      NONE => accumalate(xs',acc)
	             |SOME x => accumalate(xs',x@acc)
in
  accumalate(lst_lstStrings,[])
end

fun similar_names (lst_lstStrings,{first=f_name,middle=m_name,last=l_name}) = 
let 
  val name_list = get_substitutions2(lst_lstStrings,f_name)
  fun accumalate(lst,acc) =   
        case lst of
           [] => acc
	| x::xs' => accumalate(xs',{first=x,middle=m_name,last=l_name}::acc)
in
 accumalate(name_list,[{first=f_name,middle=m_name,last=l_name}])
end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(s,r) = 
case s of
      Clubs => Black
    | Diamonds => Red
    | Hearts => Red
    | Spades => Black

fun card_value(s,r) = 
case r of
     Num v => v
  |  Ace => 11
  |  _ => 10     

fun remove_card(cs:card list,c:card,e:exn) = 
let fun build_list(xcs,acc) = 
      case xcs of
          [] => raise e
        | x::xs' => if x = c
                   then acc@xs'
                   else build_list(xs',x::acc)
in
 build_list(cs,[])
end

fun all_same_color(cardList) = 
case cardList of
        []  => true
        |  _::[] => true
	| x1::x2::xs' => if card_color(x1)=card_color(x2) andalso all_same_color(x2::xs')
                         then true
                         else false
  
fun sum_cards(cardList) = 
let fun adder(xs,sum) = 
    case xs of 
       [] => sum
     | x::xs' => adder(xs',sum+card_value(x)) 
in
 adder(cardList,0)
end

fun score(held_cards,goal) = 
let val sum = sum_cards(held_cards)
    val same_color = all_same_color(held_cards)
    fun prelim_score() = 
          if (sum>goal)
          then 3 * (sum-goal)
          else goal-sum    
in 
 let val prelim_score = prelim_score()
  in 
   if(same_color)
   then prelim_score div 2
   else prelim_score
  end 
end
  
   
