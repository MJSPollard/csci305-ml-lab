(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Michael Pollard
* MJSPollard@gmail.com
*
***************************************************************)

(* Define your data type and functions here *)

(* definition of datatype used to represent sets *) 
datatype 'element set = Empty | Set of 'element * 'element set;

(* function that increments all ints in a list by 1 *)
fun f [] = []
  | f (x::xs) = (x+1) :: (f xs);

(* function that checks whether an element is in a set, is so, returns true*)
fun isMember e Empty = false 
  | isMember e (Set(x, xs)) = 
    if e = x then true
    else isMember e xs;

(* function that converts a list to a set *)
fun list2Set [] = Empty
  | list2Set e = 
    foldr (fn (x, xs) =>
    if (isMember x xs) then xs
    else Set(x, xs)) Empty e;

(* function that returns the union of two sets *)
fun union Empty s = s
  | union (Set(x, xs)) s =
    if isMember x s then union xs s
    else Set(x, union xs s);

(* function that returns the intersection of two sets *)
fun intersect Empty s = Empty
  | intersect (Set(x, xs)) s =
    if isMember x s then Set(x, (intersect xs s))
    else intersect xs s;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";


(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
