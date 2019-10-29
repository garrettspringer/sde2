open Printf

(* This is used to print a list of list of floats *)
let rec rowToString r =
  match r with
  | [] -> ""
  | h :: [] -> string_of_float h
  | h :: t -> string_of_float h ^ ";" ^ (rowToString t)

let rec imageToString i =
  match i with
  | [] -> ""
  | h :: t -> "[" ^ (rowToString h) ^ "];\n" ^ (imageToString t)

let pp_my_image s =
  print_string (imageToString s)

(** some 4-D (4 unit) data for simulation/debugging *)
let os1 = [1.0; -1.0; 1.0; -1.0];;
let os2 = [-1.0; -1.0; 1.0; -1.0];;
let os3 = [-1.0; -1.0; 1.0; 1.0];;

let w = [[0.; -1.; 1.; -1.]; [-1.; 0.; -1.; 1.]; [1.; -1.; 0.; -1.]; [-1.; 1.; -1.; 0.]];;
let w2 = [[0.; 1.; -1.; -1.]; [1.; 0.; -3.; 1.]; [-1.; -3.; 0.; -1.]; [-1.; 1.; -1.; 0.]];;

(** show Eqns (4) and (5) in action *)

(*# let w=hopTrain([os1]);;*)

(** Returns net activation for a single unit using our
list-based input and weight representation and Eqn (1)*)

let rec netUnit = function (inputs, weights) ->
  if inputs == [] || weights == [] (* If the inputs or weights is empty, return a 0 *)
  then 0.0
  else (* Else, pop & multiply inputs.hd and weights.hd and append that to a new list and recursively call netUnit *)
    (List.hd inputs *. List.hd weights) +. (netUnit(List.tl inputs, List.tl weights));;

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

let rec netAll = function (state, weightMatrix) ->
  if weightMatrix == [] (* If weight matrix is empty, return an empty list *)
  then [] 
  else (* Else, compute the state against the head weight matrix *)
    netUnit(state, List.hd weightMatrix) :: netAll(state, List.tl weightMatrix);;

(*let test1 = netAll os1 w;;
let () = List.iter (printf "%f ") test1;;
print_string "\n";;

let test2 = netAll os2 w;;
let () = List.iter (printf "%f ") test2;;
print_string "\n";;*)

(* Returns 'squashed' unit output. Implements Hopfield activation function
corresponding to Eqn (3) for single (-1,1) unit *)

let rec hop11Activation = function (net, oldo) ->
  if net > 0.0     (* if net activation is negative, return -1 *) 
  then 1.0
  else 
    if net < 0.0   (* if net activation is positive, return 1 *)
    then -1.0
    else oldo;;  (* if net activation is zero, return original value *)

(*let test1 = hop11Activation(-3., 1.);;
print_float test1;;
print_string "\n";;

let test2 = hop11Activation(3., 1.);;
print_float test2;;
print_string "\n";;
                           
let test3 = hop11Activation(0., 1.);;
print_float test3;;
print_string "\n";;*)

let rec nextState = function (currentState, weightMatrix) ->
  if weightMatrix == [] 
  then []
  else
    hop11Activation((List.hd(netAll(currentState, weightMatrix))), List.hd currentState) :: nextState(currentState, List.tl weightMatrix);;

(*let test1 = nextState(os1, w);;
let () = List.iter (printf "%f ") test1;;
print_string "\n";;

let test2 = nextState(os2, w);;
let () = List.iter (printf "%f ") test2;;
print_string "\n";;

let test3 = nextState(os1, w2);;
let () = List.iter (printf "%f ") test3;;
print_string "\n";;

let test4 = nextState(nextState(os1, w2), w2);;
let () = List.iter (printf "%f ") test4;;
print_string "\n";;*)