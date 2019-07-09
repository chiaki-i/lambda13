(* recognizer monad の signature *)

module type RecognizerMonad_t = sig
  type sentence_t = string list
  (* 入力文の型 *)

  type grammar_t
  (* モナドの型 *)

  val epsilon : grammar_t
  (* 使い方：epsilon *)
  (* 空の入力の文法 *)

  val (%) : grammar_t -> grammar_t -> grammar_t
  (* 使い方：g1 % g2 *)
  (* g1 の後に g2 が続くような文法 *)

  (* val ($) : grammar_t -> grammar_t -> grammar_t *)
  (* 使い方：g1 $ g2 *)
  (* g1 または g2 を表す文法 *)

  val word : string -> grammar_t
  (* 使い方：word str *)
  (* 終端記号 str を表す文法 *)

  val run : grammar_t -> sentence_t -> bool
  (* 使い方：run g sentence *)
  (* sentence が文法 g から導かれるかを判定する *)
end

(* recognizer monad *)

module RecognizerMonad : RecognizerMonad_t = struct
  type sentence_t = string list
  type grammar_t = sentence_t -> sentence_t list

  (* epsilon (return) : grammar_t *)
  let epsilon = fun sentence -> [sentence]

  (* % (bind) : grammar_t -> grammar_t -> grammar_t *)
  let (%) g1 g2 = fun sentence -> List.concat (List.map g2 (g1 sentence))

  (* word : string -> grammar_t *)
  let word str = fun sentence -> match sentence with
      [] -> []
    | s :: rest ->
	if s = str then [rest] else []

  (* run : grammar_t -> sentence_t -> bool *)
  let run g sentence = match g sentence with
      [] -> false
    | lst -> List.mem [] lst

  (* test : grammar_t -> sentence_t -> sentence_t list *)
  let test g sentence = g sentence

  (* test はデバッグや動きを見るには便利かも。これを使うときには *)
  (* RecognizerMonad の型指定 ": RecognizerMonad_t" を外す *)
end

(* 文法の例 *)

open RecognizerMonad

(* E = S V O, S = John, V = likes, O = Mary *)
let s : grammar_t = word "John"
let v : grammar_t = word "likes" (* $ word "loves" *)
let o : grammar_t = word "Mary"
let e : grammar_t = s % v % o

let sentence1 : sentence_t = ["John"; "likes"; "Mary"]
let sentence2 : sentence_t = ["John"; "likes"; "Mary"; "very"; "much"]
let sentence3 : sentence_t = ["John"; "loves"; "Mary"]

(* go : RecognizerMonad.sentence_t -> bool *)
let go sentence = run e sentence
