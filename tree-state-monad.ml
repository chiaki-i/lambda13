(* tree type *)
type 'a tree_t =
  | Empty
  | Node of 'a tree_t * 'a * 'a tree_t

(* State Monad *)
type state_t = int list
type 'a monad_t = state_t -> 'a * state_t

(* return : 'a -> 'a monad_t *)
let return (x : 'a) : 'a monad_t =
  fun lst -> (x, lst)

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t =
  fun lst -> let (v1, lst1) = x lst in
	     let (v2, lst2) = f v1 lst1 in
	     (v2, lst2)

(* run : (unit -> 'a monad_t) -> 'a *)
let run (thunk : unit -> 'a monad_t) =
  let (result, state) = thunk () [] in (* initial state is [] *)
  result

(* save : 'a -> unit monad_t *)
(* 状態への書き込み *)
let save x : unit monad_t = fun lst -> ((), x :: lst)

(* appeared : 'a -> bool monad_t *)
(* 状態からの読みだし *)
let appeared x : bool monad_t = fun lst -> (List.mem x lst, lst)

(* sum3 : 同じ数字はカウントしない *)
let rec sum3 (tree : int tree_t) : int monad_t = match tree with
  | Empty -> return 0
  | Node (left, value, right) ->
    bind (sum3 left) (fun x1 ->
        bind (appeared value) (fun b ->
            if b then
              bind (sum3 right) (fun x2 ->
                  return (x1 + x2))
            else
              bind (save value) (fun _ ->
                  bind (sum3 right) (fun x2 ->
                      return (x1 + value + x2)))))

let main3 lst = run (fun () -> sum3 lst)

(* example trees *)
let tree1 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 1, Empty)),
	1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

let tree2 =
  Node (Node (Node (Empty, 1, Empty), 3, Node (Empty, 0, Empty)),
	-1,
	Node (Node (Empty, 2, Empty), 4, Node (Empty, 3, Empty)))

let sum3_test1 = main3 tree1 = 10
let sum3_test2 = main3 tree2 = 9
