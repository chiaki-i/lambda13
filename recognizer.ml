(* recognizer monad �� signature *)

module type RecognizerMonad_t = sig
  type sentence_t = string list
  (* ����ʸ�η� *)

  type grammar_t
  (* ��ʥɤη� *)

  val epsilon : grammar_t
  (* �Ȥ�����epsilon *)
  (* �������Ϥ�ʸˡ *)

  val (%) : grammar_t -> grammar_t -> grammar_t
  (* �Ȥ�����g1 % g2 *)
  (* g1 �θ�� g2 ��³���褦��ʸˡ *)

  (* val ($) : grammar_t -> grammar_t -> grammar_t *)
  (* �Ȥ�����g1 $ g2 *)
  (* g1 �ޤ��� g2 ��ɽ��ʸˡ *)

  val word : string -> grammar_t
  (* �Ȥ�����word str *)
  (* ��ü���� str ��ɽ��ʸˡ *)

  val run : grammar_t -> sentence_t -> bool
  (* �Ȥ�����run g sentence *)
  (* sentence ��ʸˡ g ����Ƴ����뤫��Ƚ�ꤹ�� *)
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

  (* test �ϥǥХå���ư���򸫤�ˤ��������⡣�����Ȥ��Ȥ��ˤ� *)
  (* RecognizerMonad �η����� ": RecognizerMonad_t" �򳰤� *)
end

(* ʸˡ���� *)

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
