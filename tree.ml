(* 木の型 *)
type 'a tree_t = Empty
	       | Node of 'a tree_t * 'a * 'a tree_t

(* sum1 : int tree の要素の合計を返す *)
let rec sum1 (tree : int tree_t) : int = match tree with
  | Empty -> 0
  | Node (left, value, right) -> (sum1 left) + value + (sum1 right)

(* sum2 : 要素が負または 0 のとき Error 、それ以外は Success を返す *)
type 'a error_t = Error of string
		| Success of 'a

let rec sum2 (tree : int tree_t) : int error_t = match tree with
  | Empty -> Success 0
  | Node (left, value, right) ->
    if value = 0 then Error "zero"
    else if value < 0 then Error "negative"
    else
      match (sum2 left, sum2 right) with
      | (Success n, Success m) -> Success (n + value + m)
      | (Error s, _) -> Error s
      | (Success _, Error s) -> Error s

(* sum3 : 履歴を持ち歩き、同じ数字は加算しない *)
(* デバッグ用 *)
let rec print_lst (lst : int list) : unit = match lst with
  | [] -> print_string "]"
  | first :: rest -> print_int first; print_string "; "; print_lst rest

let rec sum3 (tree : int tree_t) (lst : int list) : int * int list =
  match tree with
  | Empty -> (List.fold_left (+) 0 lst, lst)
  | Node (left, value, right) ->
    let left_lst = snd (sum3 left lst) in
    if List.mem value left_lst
    then sum3 right left_lst
    else sum3 right (value :: left_lst)

(* sum4 : 候補が複数ある場合、起こりうる全ての結果をリストで返す *)
let rec power_sum (lst1 : int list) (lst2 : int list) : int list =
  match (lst1, lst2) with
  | ([], _) -> []
  | (lst1, []) -> lst1
  | (first :: rest, _ :: _) ->
    let new_lst2 = List.map ((+) first) lst2 in
    new_lst2 @ (power_sum rest lst2)

let rec sum4 (tree : int list tree_t) : int list = match tree with
  | Empty -> []
  | Node (left, lst, right) ->
    let left_lst = sum4 left in
    let middle_lst = power_sum lst left_lst in
    power_sum middle_lst (sum4 right)

(* 例 *)
let tree1 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 1, Empty)),
	1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

let tree2 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 0, Empty)),
	-1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

let tree3 =
  Node (Node (Node (Empty, [1], Empty), [3; 4], Node (Empty, [0], Empty)),
	[-1],
	Node (Node (Empty, [2], Empty), [4; 6], Node (Empty, [3], Empty)))

(* テスト *)
let sum1_test1 = sum1 tree1 = 15
let sum1_test2 = sum1 tree2 = 12

let sum2_test1 = sum2 tree1 = Success 15
let sum2_test2 = sum2 tree2 = Error "negative"

let sum3_test1 = sum3 tree1 [] = (10, [4; 2; 3; 1])
let sum3_test2 = sum3 tree2 [] = (9, [4; 2; -1; 0; 3; 1])

let power_sum_test0 = power_sum [2; 3] [] = [2; 3]
let power_sum_test1 = power_sum [1] [2; 3] = [3; 4]
let power_sum_test2 = power_sum [4; 5] [4; 6] = [8; 10; 9; 11]

let sum4_test1 = sum4 tree3 = [12; 14; 13; 15]
