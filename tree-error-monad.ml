(* tree type *)
type 'a tree_t =
  | Empty
  | Node of 'a tree_t * 'a * 'a tree_t

(*********************************************************)
(* Error Monad *)
(* option 型でエラーを模倣したものと同じ *)
type 'a monad_t =
  | Error of string
  | Success of 'a

(* return : 'a -> 'a monad_t *)
(* return は success を返すようになる *)
let return (x : 'a) : 'a monad_t = Success (x)

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t =
  match x with
      Error (s) -> Error (s)
    | Success (v) -> f v

(* run : (unit -> 'a monad_t) -> 'a *)
let run (thunk : unit -> 'a monad_t) = match thunk () with
    Error (s) -> failwith s
  | Success (v) -> v

(* error : string -> 'a monad_t *)
let error s = Error (s)

(* sum2 : 要素が負または 0 のとき Error、それ以外は Success を返す *)
let rec sum2 (tree : int tree_t) : int monad_t = match tree with
  | Empty -> return 0
  | Node (left, value, right) ->
    if value = 0 then error "zero"
    else if value < 0 then error "negative"
    else
      bind (sum2 left) (fun x1 ->
          bind (sum2 right) (fun x2 ->
              return (x1 + value + x2)))

let main2 (tree : int tree_t) : int = run (fun () -> sum2 tree)

(* example trees *)
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

let sum2_test1 = main2 tree1 = 15
let sum2_test2 =
  try
    let _ = main2 tree2 in
    false
  with Failure "negative" -> true
