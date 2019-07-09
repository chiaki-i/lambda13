(* int list の要素の合計を返す *)
(* sum1 : int list -> int *)
let rec sum1 lst = match lst with
    [] -> 0
  | i :: rest -> i + sum1 rest

(* データにエラー (0, 負) がある場合 *)

type 'a error_t = Error of string
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

(* 同じ数字はカウントしない *)

(* sum3 : int list -> int list -> int * int list *)
let rec sum3 lst history = match lst with
    [] -> (0, history)
  | i :: rest ->
      if List.mem i history
      then let (i2, history2) = sum3 rest history in
	   (i2, history2)
      else let (i2, history2) = sum3 rest (i :: history) in
	   (i + i2, history2)

(* 候補が複数ある場合 *)

(* sum4 : int list list -> int list *)
let rec sum4 lst = match lst with
    [] -> [0]
  | lst :: rest ->
      let lst1 = sum4 rest in List.concat (List.map (fun i2 -> 
	List.map (fun i -> i + i2) lst
      ) lst1)

(* 例 *)

let lst1 = [1; 3; 0; -1; 2; 4; 3]

let lst2 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]
