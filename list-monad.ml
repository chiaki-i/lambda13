(* int list の要素の合計を返す *)

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

let lst2 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]
