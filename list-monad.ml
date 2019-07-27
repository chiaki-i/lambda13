(* sum functions (sum1, sum2, sum3, sum4) over int list *)

type 'a monad_t = 'a

(* return : 'a -> 'a monad_t *)
let return (x : 'a) : 'a monad_t = x

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t = f x

(* run : (unit -> 'a monad_t) -> 'a *)
let run (thunk : unit -> 'a monad_t) : 'a = thunk ()

(* sum1 : int list -> int monad_t *)
let rec sum1 lst : int monad_t = match lst with
    [] -> return 0
  | i :: rest ->
      bind (sum1 rest) (fun i1 ->
      return (i + i1))

(* メイン *)
let main1 lst = run (fun () -> sum1 lst)

(* データにエラー (0, 負) がある場合 *)

type 'a monad_t = Error of string
		| Success of 'a

(* return : 'a -> 'a monad_t *)
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

(* sum2 : int list -> int monad_t *)
let rec sum2 lst = match lst with
    [] -> return 0
  | i :: rest ->
      if i = 0 then error "zero" else
      if i < 0 then error "negative" else
      bind (sum2 rest) (fun i2 ->
      return (i + i2))

(* メイン *)

let main2 lst = run (fun () -> sum2 lst)

(* 同じ数字はカウントしない *)

type state_t = int list
type 'a monad_t = state_t -> 'a * state_t

(* return は状態を変更しない *)
let return (x : 'a) : 'a monad_t =
  fun lst -> (x, lst)

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t =
  fun lst -> let (v1, lst1) = x lst in (* x に現在の状態を渡して実行させる *)
	     let (v2, lst2) = f v1 lst1 in (* f v1 に最新の状態 lst1 を渡す *)
	     (v2, lst2)

(* run : (unit -> 'a monad_t) -> 'a *)
let run (thunk : unit -> 'a monad_t) =
  let (result, state) = thunk () [] in
  result

(* save : 'a -> unit monad_t *)
let save x : unit monad_t = fun lst -> ((), x :: lst)

(* appeared : 'a -> bool monad_t *)
let appeared x : bool monad_t = fun lst -> (List.mem x lst, lst)

(* sum3 : int list -> int monad_t *)
let rec sum3 lst = match lst with
    [] -> return 0
  | i :: rest ->
      bind (appeared i) (fun b ->
      if b then sum3 rest
	   else bind (save i) (fun _ ->
		bind (sum3 rest) (fun i2 ->
		return (i + i2))))

(* メイン *)

let main3 lst = run (fun () -> sum3 lst)

(* 候補が複数ある場合 *)

type 'a monad_t = 'a list

(* return : 'a -> 'a monad_t *)
let return (x : 'a) : 'a monad_t = [x]

(* bind : 'a monad_t -> ('a -> 'b monad_t) -> 'b monad_t *)
let bind (x : 'a monad_t) (f : 'a -> 'b monad_t) : 'b monad_t =
  List.concat (List.map f x)

(* run1 : (unit -> 'a monad_t) -> 'a *)
(* 一つでも解が出てきたらおしまい: run all して最初の一個を返しているだけ
   もし一つもなかったら Fail してくれる *)
let run1 (thunk : unit -> 'a monad_t) =
  match thunk () with
    [] -> failwith "No answer found"
  | first :: rest -> first

(* run_all : (unit -> 'a monad_t) -> 'a list *)
let run_all (thunk : unit -> 'a monad_t) = thunk ()

(* either : 'a -> 'a -> 'a monad_t *)
let either (a : 'a) b : 'a monad_t = [a; b]

(* choice : 'a list -> 'a monad_t *)
let choice (lst : 'a list) : 'a monad_t = lst

(* fail : unit -> 'a monad_t *)
let fail () : 'a monad_t = []

(* sum4 : int list -> int monad_t *)
let rec sum4 lst : int monad_t = match lst with
    [] -> return 0
  | i :: rest ->
      bind (sum4 rest) (fun i1 ->
      return (i + i1))

(* メイン *)

(* ndlist2list : 'a list list -> 'a list monad_t *)
let rec ndlist2list lst = match lst with
    [] -> return []
  | first :: rest ->
      bind (choice first) (fun i ->
      bind (ndlist2list rest) (fun rest' ->
      return (i :: rest')))

let main4 lst =
  run1 (fun () ->
	 bind (ndlist2list lst) (fun lst' ->
	 sum4 lst'))

let main4' lst : 'a list =
  run_all (fun () ->
	 bind (ndlist2list lst) (fun lst' ->
	 sum4 lst'))

(* 例 *)

let lst1 = [1; 3; 0; -1; 2; 4; 3]

(* 本当は、このリストは monad_t を直接書き表してしまっているので、できれば
   抽象データ型で書きたい気持ちがある。しかしリストの中に monad_t を並べることは
   できないので、正確には bind を用いて書くべき。
   (なので型は 'a monad_t list ではなく 'a list monad_t がよい。
    OCaml 的には両方リストで表現されているのでどちらでも通ってしまうけど…)*)
(* bind (return 0) (fun _ ->
   bind (choice [3; 4]) (fun _ ->
   ... )) のような感じ *)
(* 下のように (簡単のために) 直接リストで書かれたものを bind を用いた形に
   変換しているのが ndlist2list という関数 *)
let lst2 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]

(* 継続モナド *)

type ('a, 'ans) monad_t = ('a -> 'ans) -> 'ans

(* return : 'a -> ('a, 'ans) monad_t *)
let return (x : 'a) : ('a, 'ans) monad_t = fun k -> k x

(* bind : ('a, 'ans) monad_t -> ('a -> ('b, 'ans) monad_t) ->
          ('b, 'ans) monad_t *)
let bind (x : ('a, 'ans) monad_t) (f : 'a -> ('b, 'ans) monad_t)
  : ('b, 'ans) monad_t = fun k -> x (fun v -> f v k)

(* callcc : (('a -> ('b, 'ans) monad_t) -> ('a, 'ans) monad_t) ->
            ('a, 'ans) monad_t *)
(* パースの法則 *)
let callcc (f : ('a -> ('b, 'ans) monad_t) -> ('a, 'ans) monad_t)
  : ('a, 'ans) monad_t = fun k -> f (fun v k' -> k v) k

(* run : (unit -> ('ans, 'ans) monad_t) -> 'ans *)
let run (thunk : unit -> ('a, 'ans) monad_t) =
  thunk () (fun x -> x)

(* sum5 : int list -> (int, 'ans) monad_t *)
let rec sum5 lst : (int, 'ans) monad_t = match lst with
    [] -> return 0
  | i :: rest ->
      bind (sum5 rest) (fun i1 ->
      return (i + i1))

(* メイン *)

let main5 lst = run (fun () -> sum5 lst)

(* 継続 parameterized モナド *)
(* ans を固定のままだと s/r にできないので変えられるようにする *)
type ('a, 's, 't) monad_t = ('a -> 's) -> 't

(* return : 'a -> ('a, 'a, 't) monad_t *)
let return (x : 'a) : ('a, 's, 's) monad_t = fun k -> k x

(* bind : ('a, 'ans) monad_t -> ('a -> ('b, 'ans) monad_t) ->
          ('b, 'ans) monad_t *)
let bind (x : ('a, 't, 'u) monad_t) (f : 'a -> ('b, 's, 't) monad_t)
  : ('b, 's, 'u) monad_t = fun k -> x (fun v -> f v k)

(* reset : ('a, 'a, 's) monad_t -> ('s, 't, 't) monad_t *)
let reset (f : ('a, 'a, 's) monad_t) : ('s, 't, 't) monad_t
  = fun k -> k (f (fun x -> x))

(* shift : (('p -> ('a, 't, 't) monad_t) -> ('s, 's, 'b) monad_t) ->
           ('p, 'a, 'b) monad_t *)
let shift (f : ('p -> ('a, 't, 't) monad_t) -> ('s, 's, 'b) monad_t)
  : ('p, 'a, 'b) monad_t = fun k -> f (fun v k' -> k' (k v)) (fun x -> x)
(* k' (k v) と書いているので、
   k' と k が direct style で compose されている *)
