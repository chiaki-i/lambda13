(* parser monad の signature *)

module type ParserMonad_t = sig
  type sentence_t = string list
  (* 入力文の型 *)

  type 'a parser_t
  (* モナドの型 *)

  val epsilon : 'a -> 'a parser_t
  (* 使い方：epsilon x *)
  (* 空の入力の文法。x をそのまま返す *)

  val (%) : 'a parser_t -> ('a -> 'b parser_t) -> 'b parser_t
  (* 使い方：g1 % g2 *)
  (* g1 の後に g2 が続くような文法 *)

  val ($) : 'a parser_t -> 'a parser_t -> 'a parser_t
  (* 使い方：g1 $ g2 *)
  (* g1 または g2 を表す文法 *)

  val word : string -> string parser_t
  (* 使い方：word str *)
  (* 終端記号 str を表す文法 *)

  val number : int parser_t
  (* 使い方：number *)
  (* 数字を表す文法 *)

  val run : 'a parser_t -> sentence_t -> 'a list
  (* 使い方：run p sentence *)
  (* sentence をパーザ p で構文解析する *)
end

(* parser monad *)

module ParserMonad : ParserMonad_t = struct
  type sentence_t = string list
  type 'a parser_t = sentence_t -> ('a * sentence_t) list

  (* epsilon (return) : 'a -> 'a parser_t *)
  let epsilon x = fun sentence -> [(x, sentence)]

  (* % (bind) : 'a parser_t -> ('a -> 'b parser_t) -> 'b parser_t *)
  let (%) p f = fun sentence ->
    List.concat (List.map (fun (x, sentence') -> f x sentence')
			  (p sentence))

  (* $ : 'a parser_t -> 'a parser_t -> 'a parser_t *)
  let ($) p1 p2 = fun sentence -> p1 sentence @ p2 sentence

  (* word : string -> 'a parser_t *)
  let word str = fun sentence -> match sentence with
      [] -> []
    | s :: rest ->
	if s = str then [(s, rest)] else []

  (* number : 'a parser_t *)
  let number = fun sentence -> match sentence with
      [] -> []
    | s :: rest ->
	try
	  let i = int_of_string s in [(i, rest)]
	with Failure "int_of_string" -> []

  (* run : 'a parser_t -> sentence_t -> 'a list *)
  let run p sentence =
    let results = p sentence in
    List.map fst (List.filter (fun (x, lst) -> lst = []) results)
end

(* 文法の例 *)

open ParserMonad

(* E = S V O, S = John, V = likes, O = Mary *)
let s () = word "John"
let v () = word "likes" $ word "loves"
let o () = word "Mary"
let e () = s () % fun s' ->
	   v () % fun v' ->
	   o () % fun o' ->
	   epsilon (s' ^ " " ^ v' ^ " " ^ o')

let sentence1 : sentence_t = ["John"; "likes"; "Mary"]
let sentence2 : sentence_t = ["John"; "likes"; "Mary"; "very"; "much"]
let sentence3 : sentence_t = ["John"; "loves"; "Mary"]

(* go1 : sentence_t -> string list *)
let go1 sentence = run (e ()) sentence

(* 四則演算式の文法の例 *)

(* E = E + E | E * E | ( E ) | 数字 *)

(* let rec e () =
     (e () % fun e1 ->
      word "+" % fun plus ->
      e () % fun e2 ->
      epsilon (e1 + e2))
   $ (e () % fun e1 ->
      word "*" % fun times ->
      e () % fun e2 ->
      epsilon (e1 * e2))
   $ (word "(" % fun lparen ->
      e () % fun e1 ->
      word ")" % fun rparen ->
      epsilon e1)
   $ number
*)

(* 上のように書きたいが、これだと左再帰しているので止まらない *)
(* 代わりに以下の文法にする *)

(* E = T E'
   E'= + T E' | ε
   T = A T'
   T'= * A T' | ε
   A = 0 | ( E )
*)

let rec e  () =  t ()     % fun t1 ->
		 e' ()    % fun e1 ->
		 epsilon (t1 + e1)
and     e' () = (word "+" % fun _ ->
		 t ()     % fun t1 ->
		 e' ()    % fun e2 ->
		 epsilon (t1 + e2))
	      $  epsilon 0
and     t  () =  a ()     % fun a1 ->
		 t' ()    % fun t1 ->
		 epsilon (a1 * t1)
and     t' () = (word "*" % fun _ ->
		 a ()     % fun a1 ->
		 t' ()    % fun t2 ->
		 epsilon (a1 * t2))
	      $  epsilon 1
and     a  () =  number
	      $  word "(" % fun _ ->
		 e ()     % fun e1 ->
		 word ")" % fun _ ->
		 epsilon e1

let formula1 = ["3"; "+"; "5"; "*"; "2"]
let formula2 = ["("; "3"; "+"; "5"; ")"; "*"; "2"]
let formula3 = ["1"; "+"; "2"; "+"; "3"; "+"; "4"; "+"; "5"; "+"; "6"]

(* go2 : sentence_t -> int list *)
let go2 sentence = run (e ()) sentence
