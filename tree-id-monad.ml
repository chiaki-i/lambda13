(* tree type *)
type 'a tree_t =
  | Empty
  | Node of 'a tree_t * 'a * 'a tree_t

(* Identity Monad *)
type 'a monad_t = 'a

(* return : 'a -> 'a monad_t *)
let return (x : 'a) : 'a monad_t = x

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t = f x

(* run : (unit -> 'a monad_t) -> 'a *)
let run (thunk : unit -> 'a monad_t) : 'a = thunk ()

(* sum1 : int tree の要素の合計を返す
   定義から 'a monad_t 型は 'a 型をあらわすので、 return モナドを使わなくても
   型チェックには通る *)
let rec sum1 (tree : int tree_t) : int monad_t = match tree with
  | Empty -> return 0
  | Node (left, value, right) ->
    bind (sum1 left) (fun x1 ->
        bind (sum1 right) (fun x2 ->
            return (x1 + value + x2)))

let main1 (tree : int tree_t) = run (fun () -> sum1 tree)

(* example trees *)
let tree1 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 1, Empty)),
	1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

let tree2 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 0, Empty)),
	-1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

(* tests *)
let sum1_test1 = main1 tree1 = 15
let sum1_test2 = main1 tree2 = 12
