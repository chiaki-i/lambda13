(* identity monad �� signature *)

module type Monad_t = sig
  type 'a t
  (* 'a ���Υ�ʥɤη� *)

  val return : 'a -> 'a t
  (* �Ȥ�����return v *)
  (* 'a ������ v ���ʥɤ������� *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* �Ȥ�����bind m1 (fun x -> m2) *)
  (* 'a t ���� m1 ���� 'a ������ x ����Ф��ơ������Ȥä�
     'b ���Υ�ʥɤ��롣m1 �η�̤� x ������� m2 ��׻����� *)

  val run : (unit -> 'a t) -> 'a
  (* �Ȥ�����run (fun () -> m) *)
  (* 'a t ���� m ��¹Ԥ��ơ���̤����� *)
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

(* error monad �� signature *)

module type ErrorMonad_t = sig
  type 'a t
  (* 'a ���Υ�ʥɤη� *)

  val return : 'a -> 'a t
  (* �Ȥ�����return v *)
  (* 'a ������ v ���ʥɤ������� *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* �Ȥ�����bind m1 (fun x -> m2) *)
  (* 'a t ���� m1 ���� 'a ������ x ����Ф��ơ������Ȥä�
     'b ���Υ�ʥɤ��롣m1 �η�̤� x ������� m2 ��׻����� *)

  val run : (unit -> 'a t) -> 'a
  (* �Ȥ�����run (fun () -> m) *)
  (* 'a t ���� m ��¹Ԥ��ơ���̤����� *)

  val error : string -> 'a t
  (* �Ȥ�����error "str" *)
  (* 'a ������ v ���ʥɤ������� *)
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

(* state monad �� signature *)

module type StateMonad_t = sig
  type 'a t
  (* 'a ���Υ�ʥɤη� *)

  val return : 'a -> 'a t
  (* �Ȥ�����return v *)
  (* 'a ������ v ���ʥɤ������� *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* �Ȥ�����bind m1 (fun x -> m2) *)
  (* 'a t ���� m1 ���� 'a ������ x ����Ф��ơ������Ȥä�
     'b ���Υ�ʥɤ��롣m1 �η�̤� x ������� m2 ��׻����� *)

  val run : (unit -> 'a t) -> 'a
  (* �Ȥ�����run (fun () -> m) *)
  (* 'a t ���� m ��¹Ԥ��ơ���̤����� *)

  val save : int -> unit t
  (* �Ȥ�����save i *)
  (* i �����줿���Ȥ�Ͽ���� *)

  val appeared : int -> bool t
  (* �Ȥ�����appeared i *)
  (* i �����˸��줿���ɤ������֤� *)
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

(* list monad �� signature *)

module type ListMonad_t = sig
  type 'a t
  (* 'a ���Υ�ʥɤη� *)

  val return : 'a -> 'a t
  (* �Ȥ�����return v *)
  (* 'a ������ v ���ʥɤ������� *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (* �Ȥ�����bind m1 (fun x -> m2) *)
  (* 'a t ���� m1 ���� 'a ������ x ����Ф��ơ������Ȥä�
     'b ���Υ�ʥɤ��롣m1 �η�̤� x ������� m2 ��׻����� *)

  val run : (unit -> 'a t) -> 'a
  (* �Ȥ�����run (fun () -> m) *)
  (* 'a t ���� m ��¹Ԥ��ơ���̤����� *)

  val either : 'a -> 'a -> 'a t
  (* �Ȥ�����either x y *)
  (* x �� y �������Ū���֤� *)

  val choice : 'a list -> 'a t
  (* �Ȥ�����choice [x; y; z; ...] *)
  (* x �� y �� z ���Ĥ������Ū���֤� *)

  val fail : unit -> 'a t
  (* �Ȥ�����fail () *)
  (* ���ߤη׻��ϼ��Ԥ����Ȥ������Ǥ��� *)
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

(* �� *)

let lst1 = [1; 3; 0; -1; 2; 4; 3]
let lst2 = [1; 3;    -1; 2; 4; 3]
let lst3 = [1; 3;        2; 4; 3]
let lst4 = [[1]; [3; 4]; [0]; [-1]; [2]; [4; 6]; [3]]

(* int list �����Ǥι�פ��֤� *)

module Main1 (Monad : Monad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst : int Monad.t = match lst with
      [] -> Monad.return 0
    | i :: rest ->
	Monad.bind (sum rest) (fun i1 ->
	  Monad.return (i + i1))

  (* �ᥤ��ؿ� *)
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

(* �ǡ����˥��顼 (0, ��) �������� *)

module Main2 (Monad : ErrorMonad_t) = struct
  (* sum : int list -> int Monad.t *)
  let rec sum lst = match lst with
      [] -> Monad.return 0
    | i :: rest ->
	if i = 0 then Monad.error "zero" else
	  if i < 0 then Monad.error "negative" else
	    Monad.bind (sum rest) (fun i2 ->
	      Monad.return (i + i2))

  (* �ᥤ��ؿ� *)
  (* Main.f : int list -> int *)
  let f lst = Monad.run (fun () -> sum lst)
end

module M2e = Main2 (ErrorMonad)
(* let test2e_1 = M2e.f lst1 *)
(* Exception: Failure "zero". *)
(* let test2e_2 = M2e.f lst2 *)
(* Exception: Failure "negative". *)
let test2e_3 = M2e.f lst3 = 13

(* Ʊ�������ϥ�����Ȥ��ʤ� *)

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

  (* �ᥤ��ؿ� *)
  (* Main.f : int list -> int *)
  let f lst = Monad.run (fun () -> sum lst)
end

module M3s = Main3 (StateMonad)
let test3s_1 = M3s.f lst1 = 9
let test3s_2 = M3s.f lst2 = 9
let test3s_3 = M3s.f lst3 = 10

(* ���䤬ʣ�������� *)

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

  (* �ᥤ��ؿ� *)
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
