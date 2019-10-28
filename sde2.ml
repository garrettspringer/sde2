(*let average a b =
  print_string "Hello, World\n";
  (a +. b) /. 2.0;;
let i = average 10. 20.;;
print_float i;;*)

(*let rec range a b =
    if a > b then []
    else a :: range (a+1) b;; (* :: constructs a list with the left side being the head and right being the tail *)

let a = range 1 10;;
open Printf
let () = List.iter (printf "%d ") a*)

open Printf
(* This is used to print a list of list of floats *)
(*let rec rowToString r =
  match r with
  | [] -> ""
  | h :: [] -> string_of_float h
  | h :: t -> string_of_float h ^ ";" ^ (rowToString t)

let rec imageToString i =
  match i with
  | [] -> ""
  | h :: t -> "[" ^ (rowToString h) ^ "];\n" ^ (imageToString t)

let pp_my_image s =
  print_string (imageToString s)*)

(** some 4-D (4 unit) data for simulation/debugging *)
let os1 = [1.0; -1.0; 1.0; -1.0];;
let os2 = [-1.0; -1.0; 1.0; -1.0];;
let os3 = [-1.0; -1.0; 1.0; 1.0];;

(** show Eqns (4) and (5) in action *)

(*# let w=hopTrain([os1]);;*)

(** Returns net activation for a single unit using our
list-based input and weight representation and Eqn (1)*)

let rec netUnit inputs weights =
  if inputs == [] || weights == [] (* If the inputs or weights is empty, return a 0 *)
  then 0.0
  else (* Else, pop & multiply inputs.hd and weights.hd and append that to a new list and recursively call netUnit *)
    (List.hd inputs *. List.hd weights) +. (netUnit (List.tl inputs) (List.tl weights));;

(*let test1 = netUnit [-1.; -1.] [1.; 0.];;
print_float test1;;
print_string "\n";;

let test2 = netUnit [-1.; -1.; 1.; -1.] [1.; 0.; -3.; 1.];;
print_float test2;;
print_string "\n";;

let test3 = netUnit os1 [1.;2.;3.;1.];;
print_float test3;;
print_string "\n";;*)

(* Returns net activation computation for entire network
as a vector (list) of individual unit activations *)

let rec netAll state weightMatrix =
  if weightMatrix == [] (* If weight matrix is empty, return an empty list *)
  then [] 
  else (* Else, compute the state against the head weight matrix *)
    (netUnit (state) (List.hd weightMatrix)) :: (netAll (state) (List.tl weightMatrix));;

let w = [[0.; -1.; 1.; -1.]; [-1.; 0.; -1.; 1.]; [1.; -1.; 0.; -1.]; [-1.; 1.; -1.; 0.]];;

(*let test1 = netAll os1 w;;
let () = List.iter (printf "%f ") test1;;
print_string "\n";;

let test2 = netAll os2 w;;
let () = List.iter (printf "%f ") test2;;
print_string "\n";;*)
