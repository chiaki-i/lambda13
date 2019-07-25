(* tree type *)
type 'a tree_t =
  | Empty
  | Node of 'a tree_t * 'a * 'a tree_t

(* Non-deterministic Monad *)
type 'a monad_t = 'a list

(* return : 'a -> 'a monad_t *)
let return (x : 'a) : 'a monad_t = [x] (* 答えの候補が並んでいるリスト *)

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t =
  List.concat (List.map f x)

(* run1 : (unit -> 'a monad_t) -> 'a *)
(* run_all をしてとりあえず最初の要素を答えとして返しているだけ *)
let run1 (thunk : unit -> 'a monad_t) =
  match thunk () with
    [] -> failwith "No answer found"
  | first :: rest -> first

(* run_all : (unit -> 'a monad_t) -> 'a list *)
let run_all (thunk : unit -> 'a monad_t) = thunk ()

(* either : 'a -> 'a -> 'a monad_t *)
(* 二つの計算結果を受け取ってきたら、どちらも候補としてリストに登録される *)
let either (a : 'a) (b : 'a) : 'a monad_t = [a; b]

(* choice : 'a list -> 'a monad_t *)
let choice (lst : 'a list) : 'a monad_t = lst

(* fail : unit -> 'a monad_t *)
(* "候補となる結果はありませんでした" *)
let fail () : 'a monad_t = []

(* sum4 : 候補が複数あるとき *)
let rec sum4 (tree : int tree_t) : int monad_t = match tree with
  | Empty -> return 0
  | Node (left, value, right) ->
    bind (sum4 left) (fun x1 ->
        bind (sum4 right) (fun x2 ->
          return (x1 + value + x2)))

(* to_monad : 'a list tree_t -> 'a monad_t *)
let rec to_monad (tree : 'a list tree_t) : 'a tree_t monad_t =
  match tree with
  | Empty -> return Empty
  | Node (left, value, right) ->
      bind (choice value) (fun i ->
        bind (to_monad left) (fun x1 ->
            bind (to_monad right) (fun x2 ->
              return (Node (x1, i, x2)))))

let main4 lst =
  run1 (fun () ->
	  bind (to_monad lst) (fun tree ->
	      sum4 tree))

let main4' lst : 'a list =
  run_all (fun () ->
	  bind (to_monad lst) (fun tree ->
	      sum4 tree))

(* example trees *)
let tree3 =
  Node (Node (Node (Empty, [1], Empty), [3; 4], Node (Empty, [0], Empty)),
	[-1],
	    Node (Node (Empty, [2], Empty), [4; 6], Node (Empty, [3], Empty)))

(* tests *)
let sum4_test1 = main4 tree3 = 12
let sum4_test2 = main4' tree3 = [12; 14; 13; 15]
