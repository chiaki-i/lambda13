(* �ڤη� *)
type 'a tree_t = Empty
	       | Node of 'a tree_t * 'a * 'a tree_t

(* int tree �����Ǥι�פ��֤� *)
(* sum1 : int tree_t -> int *)
let rec sum1 tree = ...

(* �ǡ����˥��顼 (0, ��) �������� *)

type 'a error_t = Error of string
		| Success of 'a

(* sum2 : int tree_t -> int error_t *)
let rec sum2 tree = ...

(* Ʊ�������ϥ�����Ȥ��ʤ� *)

(* sum3 : int tree_t -> int list -> int * int list *)
let rec sum3 tree lst = ...

(* ���䤬ʣ�������� *)

(* sum4 : int list tree_t -> int list *)
let rec sum4 tree = ...

(* �� *)

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
