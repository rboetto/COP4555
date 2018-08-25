// I certify that this work is my own
// Ricardo Boetto 2519915
// Principles of Programming Languages, Homework 3
// Due: 6/5/2018


(* Write a curried F# function find x y that checks whether x is in list y, which returns true 
 when x is in y and false otherwise *)
let rec find x = fun y -> if List.isEmpty y then false elif x = List.head y then true else find x (List.tail y);;

(* Write an F# program removedup x that removes duplicated elements in list x *)
let rec removedup x = if List.isEmpty x then [] elif find (List.head x) (List.tail x) then removedup (List.tail x) else List.head x :: removedup (List.tail x);;

(* Write a curried F# function cartesian xs ys that takes as input two lists xs and ys 
 and returns a list of pairs that represents the Cartesian product of xs and ys *)
let rec cartesian xs = fun ys -> match xs, ys with
| [], _ -> [];
| _, [] -> [];
| [x], y::ys -> (x, y) :: cartesian [x] ys;
| x::xs, ys -> cartesian [x] ys @ cartesian xs ys;;

(* Write an F# function powerset such that powerset set returns the set of all subsets of set.*)
let rec powerset = function
    | [] -> [[]]
    | x::xs -> List.map (fun y -> x::y) (powerset xs) @  (powerset xs) 

(* Write an efficient F# function to compute the transpose of an m-by-n matrix *)
let rec transpose xs = if List.isEmpty (List.tail (List.head xs)) then [List.map List.head xs] else [List.map List.head xs] @ transpose (List.map List.tail xs);;