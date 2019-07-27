(* sum functions (sum1, sum2, sum3, sum4) over int list *)

(* sum1 : int list -> int *)
let rec sum1 lst = match lst with
    [] -> 0
  | i :: rest -> i + sum1 rest

(* Error when elements are 0 or negative, Success otherwise *)
type 'a error_t =
    Error of string
  | Success of 'a

(* sum2 : int list -> int error_t *)
let rec sum2 lst = match lst with
    [] -> Success (0)
  | i :: rest ->
      if i = 0 then Error "zero" else
      if i < 0 then Error "negative" else
      match sum2 rest with
	  Error (s) -> Error (s)
	| Success (i2) -> Success (i + i2)

(* keep track of added numbers, only add different numbers *)
(* sum3 : int list -> int list -> int * int list *)
let rec sum3 lst history = match lst with
    [] -> (0, history)
  | i :: rest ->
      if List.mem i history
      then let (i2, history2) = sum3 rest history in
	   (i2, history2)
      else let (i2, history2) = sum3 rest (i :: history) in
	   (i + i2, history2)

(* multiple possible answers *)
(* corresponds to non-deterministic monad *)
(* sum4 : int list list -> int list *)
let rec sum4 lst = match lst with
    [] -> [0]
  | lst :: rest ->
      let lst1 = sum4 rest in List.concat (List.map (fun i2 -> 
	List.map (fun i -> i + i2) lst
      ) lst1)

(* examples *)
let lst1 = [1; 3; 0; -1; 2; 4; 3]
let lst2 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]

(* tests *)
let test1_lst1 = sum1 lst1 = 12
let test2_lst1 = sum2 lst1 = Error "zero"
let test3_lst1 = sum3 lst1 [] = (9, [4; 2; -1; 0; 3; 1])
let test4_lst2 = sum4 lst2 = [12; 13; 14; 15]
