(* 木の型 *)
type 'a tree_t = Empty
	       | Node of 'a tree_t * 'a * 'a tree_t

(* int tree の要素の合計を返す *)
(* sum1 : int tree_t -> int *)
let rec sum1 tree = ...

(* データにエラー (0, 負) がある場合 *)

type 'a error_t = Error of string
		| Success of 'a

(* sum2 : int tree_t -> int error_t *)
let rec sum2 tree = ...

(* 同じ数字はカウントしない *)

(* sum3 : int tree_t -> int list -> int * int list *)
let rec sum3 tree lst = ...

(* 候補が複数ある場合 *)

(* sum4 : int list tree_t -> int list *)
let rec sum4 tree = ...

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
