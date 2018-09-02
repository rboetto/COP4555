(*  Ricardo Boetto 2519915
    Homework Assignment #4
    I certify that this is my own work
    Principles of Programming Languages, Summer A
    Dr. He *)


let rec length = function
    | [] -> 0
    | _::xs -> 1 + length xs

// Problem 1:
let rec inner (x: int list) = function (y: int list) -> match x, y with
    | _, [] -> 0;
    | [], _ -> 0;
    | x::xs, y::ys -> (x * y) + inner xs ys;;

// Problem 2:
let rec transpose x = match x with
    | [] -> [];
    | x::xs -> if List.isEmpty (List.tail x) then 
                [List.map List.head (x::xs)] else
                [List.map List.head (x::xs)] @ transpose (List.map List.tail (x::xs));;

let multiply (x, y) =
    let rec auxtwo p q = match q with
        | [] -> []
        | q::qs -> inner p q :: auxtwo p qs
    let rec auxone r s = match r with
        | [] -> []
        | r::rs -> auxtwo r s :: auxone rs s
    auxone x (transpose y);;

// Problem 3:
let lengthsort x =

    let rec lessthan a b = match b with
        | [] -> []
        | (x, y)::z -> if x <= a then (x, y) :: lessthan a z else lessthan a z

    let rec morethan a b = match b with
        | [] -> []
        | (x, y)::z -> if x > a then (x, y) :: morethan a z else morethan a z

    let rec quicksort x = match x with
        | [] -> []
        | (x, y)::z -> quicksort (lessthan x z) @ (x, y) :: quicksort (morethan x z);

    List.map (fun y -> List.length y, y) x |> quicksort;;

// Problem 4:
type Expr = 
    | Num of integer : int
    | Neg of Expr : Expr
    | Add of Exprs : Expr * Expr
    | Sub of Exprs : Expr * Expr
    | Prod of Exprs : Expr * Expr
    | Div of Exprs : Expr * Expr

let rec evaluate = function
    | Num n -> Some n
    | Neg e -> match evaluate e with
        | Some n -> Some (-n)
        | None -> None
    | Add (a, b) -> match evaluate a, evaluate b with
        | None, _ -> None
        | _, None -> None
        | Some a, Some b -> Some (a + b)
    | Sub (a, b) -> match evaluate a, evaluate b with
        | None, _ -> None
        | _, None -> None
        | Some a, Some b -> Some (a - b)
    | Prod (a , b) -> match evaluate a, evaluate b with
        | None, _ -> None
        | _, None -> None
        | Some a, Some b -> Some (a * b)
    | Div (a, b) -> match evaluate a, evaluate b with
        | _, None -> None
        | None, _ -> None
        | _, Some 0 -> None
        | Some a, Some b -> Some (a / b)