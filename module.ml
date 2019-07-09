(* identity monad の signature *)

module type Monad_t = sig
  type 'a t
  (* 'a 型のモナドの型 *)

  val return : 'a -> 'a t
  (* 使い方：return v *)
  (* 'a 型の値 v をモナドに埋め込む *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* 使い方：bind m1 (fun x -> m2) *)
  (* 'a t 型の m1 から 'a 型の値 x を取り出して、それを使って
     'b 型のモナドを作る。m1 の結果を x に入れて m2 を計算する *)

  val run : (unit -> 'a t) -> 'a
  (* 使い方：run (fun () -> m) *)
  (* 'a t 型の m を実行して、結果を得る *)
end

(* identity monad *)

module IdMonad = struct
  type 'a t = 'a

  (* return : 'a -> 'a t *)
  let return (x : 'a) : 'a t = x

  (* bind : 'a t -> ('a -> 'b t) -> 'b t *)
  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t = f x

  (* run : (unit -> 'a t) -> 'a *)
  let run (thunk : unit -> 'a t) : 'a = thunk ()
end

(* error monad の signature *)

module type ErrorMonad_t = sig
  type 'a t
  (* 'a 型のモナドの型 *)

  val return : 'a -> 'a t
  (* 使い方：return v *)
  (* 'a 型の値 v をモナドに埋め込む *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* 使い方：bind m1 (fun x -> m2) *)
  (* 'a t 型の m1 から 'a 型の値 x を取り出して、それを使って
     'b 型のモナドを作る。m1 の結果を x に入れて m2 を計算する *)

  val run : (unit -> 'a t) -> 'a
  (* 使い方：run (fun () -> m) *)
  (* 'a t 型の m を実行して、結果を得る *)

  val error : string -> 'a t
  (* 使い方：error "str" *)
  (* 'a 型の値 v をモナドに埋め込む *)
end

(* error monad *)

module ErrorMonad : ErrorMonad_t = struct
  type 'a t = Error of string
	    | Success of 'a

  (* return : 'a -> 'a t *)
  let return (x : 'a) : 'a t = Success (x)

  (* bind : 'a t -> ('a -> 'b t) -> 'b t *)
  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    match x with
	Error (s) -> Error (s)
      | Success (v) -> f v

  (* run : (unit -> 'a t) -> 'a *)
  let run (thunk : unit -> 'a t) = match thunk () with
      Error (s) -> failwith s
    | Success (v) -> v

  (* error : string -> 'a t *)
  let error s = Error (s)
end

(* state monad の signature *)

module type StateMonad_t = sig
  type 'a t
  (* 'a 型のモナドの型 *)

  val return : 'a -> 'a t
  (* 使い方：return v *)
  (* 'a 型の値 v をモナドに埋め込む *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* 使い方：bind m1 (fun x -> m2) *)
  (* 'a t 型の m1 から 'a 型の値 x を取り出して、それを使って
     'b 型のモナドを作る。m1 の結果を x に入れて m2 を計算する *)

  val run : (unit -> 'a t) -> 'a
  (* 使い方：run (fun () -> m) *)
  (* 'a t 型の m を実行して、結果を得る *)

  val save : int -> unit t
  (* 使い方：save i *)
  (* i が現れたことを記録する *)

  val appeared : int -> bool t
  (* 使い方：appeared i *)
  (* i が過去に現れたかどうかを返す *)
end

(* state monad *)

module StateMonad : StateMonad_t = struct
  type state_t = int list
  type 'a t = state_t -> 'a * state_t

  (* return : 'a -> 'a t *)
  let return (x : 'a) : 'a t =
    fun lst -> (x, lst)

  (* bind : 'a t -> ('a -> 'b t) -> 'b t *)
  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    fun lst -> let (v1, lst1) = x lst in
	       let (v2, lst2) = f v1 lst1 in
	       (v2, lst2)

  (* run : (unit -> 'a t) -> 'a *)
  let run (thunk : unit -> 'a t) =
    let (result, lst) = thunk () [] in
    result

  (* save : int -> unit t *)
  let save x : unit t = fun lst -> ((), x :: lst)

  (* appeared : int -> bool t *)
  let appeared x : bool t = fun lst -> (List.mem x lst, lst)
end

(* list monad の signature *)

module type ListMonad_t = sig
  type 'a t
  (* 'a 型のモナドの型 *)

  val return : 'a -> 'a t
  (* 使い方：return v *)
  (* 'a 型の値 v をモナドに埋め込む *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* 使い方：bind m1 (fun x -> m2) *)
  (* 'a t 型の m1 から 'a 型の値 x を取り出して、それを使って
     'b 型のモナドを作る。m1 の結果を x に入れて m2 を計算する *)

  val run : (unit -> 'a t) -> 'a
  (* 使い方：run (fun () -> m) *)
  (* 'a t 型の m を実行して、結果を得る *)

  val either : 'a -> 'a -> 'a t
  (* 使い方：either x y *)
  (* x か y を非決定的に返す *)

  val choice : 'a list -> 'a t
  (* 使い方：choice [x; y; z; ...] *)
  (* x か y か z か…を非決定的に返す *)

  val fail : unit -> 'a t
  (* 使い方：fail () *)
  (* 現在の計算は失敗したとして中断する *)
end

(* list monad *)

module ListMonad : ListMonad_t = struct
  type 'a t = 'a list

  (* return : 'a -> 'a t *)
  let return (x : 'a) : 'a t = [x]

  (* bind : 'a t -> ('a -> 'b t) -> 'b t *)
  let bind (x : 'a t) (f : 'a -> 'b t) : 'b t =
    List.concat (List.map f x)

  (* run : (unit -> 'a t) -> 'a *)
  let run (thunk : unit -> 'a t) =
    match thunk () with
      [] -> failwith "No answer found"
    | first :: rest -> first

  (* run_all : (unit -> 'a t) -> 'a list *)
  let run_all (thunk : unit -> 'a t) = thunk ()

  (* either : 'a -> 'a -> 'a t *)
  let either (a : 'a) b : 'a t = [a; b]

  (* choice : 'a list -> 'a t *)
  let choice (lst : 'a list) : 'a t = lst

  (* fail : unit -> 'a t *)
  let fail () : 'a t = []
end

(* 例 *)

let lst1 = [1; 3; 0; -1; 2; 4; 3]
let lst2 = [1; 3;    -1; 2; 4; 3]
let lst3 = [1; 3;        2; 4; 3]
let lst4 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]

(* int list の要素の合計を返す *)

module Main1 (Monad : Monad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst : int Monad.t = match lst with
      [] -> Monad.return 0
    | i :: rest ->
	Monad.bind (sum rest) (fun i1 ->
	  Monad.return (i + i1))

  (* メイン関数 *)
  (* Main.f : int list -> int *)
  let f lst = Monad.run (fun () -> sum lst)
end

module M1i = Main1 (IdMonad)
let test1i_1 = M1i.f lst1 = 12
let test1i_2 = M1i.f lst2 = 12
let test1i_3 = M1i.f lst3 = 13

module M1e = Main1 (ErrorMonad)
let test1e_1 = M1e.f lst1 = 12
let test1e_2 = M1e.f lst2 = 12
let test1e_3 = M1e.f lst3 = 13

module M1s = Main1 (StateMonad)
let test1s_1 = M1s.f lst1 = 12
let test1s_2 = M1s.f lst2 = 12
let test1s_3 = M1s.f lst3 = 13

module M1l = Main1 (ListMonad)
let test1l_1 = M1l.f lst1 = 12
let test1l_2 = M1l.f lst2 = 12
let test1l_3 = M1l.f lst3 = 13

(* データにエラー (0, 負) がある場合 *)

module Main2 (Monad : ErrorMonad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst = match lst with
      [] -> Monad.return 0
    | i :: rest ->
	if i = 0 then Monad.error "zero" else
	  if i < 0 then Monad.error "negative" else
	    Monad.bind (sum rest) (fun i2 ->
	      Monad.return (i + i2))

  (* メイン関数 *)
  (* Main.f : int list -> int *)
  let f lst = Monad.run (fun () -> sum lst)
end

module M2e = Main2 (ErrorMonad)
(* let test2e_1 = M2e.f lst1 *)
(* Exception: Failure "zero". *)
(* let test2e_2 = M2e.f lst2 *)
(* Exception: Failure "negative". *)
let test2e_3 = M2e.f lst3 = 13

(* 同じ数字はカウントしない *)

module Main3 (Monad : StateMonad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst = match lst with
      [] -> Monad.return 0
    | i :: rest ->
	Monad.bind (Monad.appeared i) (fun b ->
	  if b then sum rest
	       else Monad.bind (Monad.save i) (fun _ ->
		    Monad.bind (sum rest) (fun i2 ->
		    Monad.return (i + i2))))

  (* メイン関数 *)
  (* Main.f : int list -> int *)
  let f lst = Monad.run (fun () -> sum lst)
end

module M3s = Main3 (StateMonad)
let test3s_1 = M3s.f lst1 = 9
let test3s_2 = M3s.f lst2 = 9
let test3s_3 = M3s.f lst3 = 10

(* 候補が複数ある場合 *)

module Main4 (Monad : ListMonad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst = match lst with
    [] -> Monad.return 0
  | i :: rest ->
      Monad.bind (sum rest) (fun i1 ->
      Monad.return (i + i1))

  (* ndlist2list : 'a list list -> 'a list Monad.t *)
  let rec ndlist2list lst = match lst with
      [] -> Monad.return []
    | first :: rest ->
	Monad.bind (Monad.choice first) (fun i ->
	Monad.bind (ndlist2list rest) (fun rest' ->
	Monad.return (i :: rest')))

  (* メイン関数 *)
  (* Main.f : int list list -> unit *)
  let f lst =
    Monad.run (fun () ->
		Monad.bind (ndlist2list lst) (fun lst' ->
		Monad.bind (sum lst') (fun result ->
		print_int result;
		print_string " ";
		Monad.return ())));
    print_newline ()
end

module M4l = Main4 (ListMonad)
let test4s_4 = M4l.f lst4 = ()
(* 12 14 13 15 *)
