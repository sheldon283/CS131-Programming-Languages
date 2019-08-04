(* Test cases for make_matcher *)
let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type awksub_nonterminals = 
  | SENTENCE | NP | VP | ARTICLE | NOUN | VERB

let awkish_grammar = 
  (SENTENCE,
    function
    | SENTENCE -> 
      [[N NP; N VP]]
    | NP ->
      [[N ARTICLE; N NOUN]]
    | VP ->
      [[N VERB]]
    | ARTICLE ->
      [[T"A"]; [T"The"]]
    | NOUN -> 
      [[T"dog"]; [T"cat"]]
    | VERB ->
      [[T"runs"]; [T"meow"]])


let make_matcher_test = ((make_matcher awkish_grammar accept_all ["The"; "dog"; "runs"]) = Some [])

let small_frag = ["The"; "cat"; "runs"]
let match_parser_test =
  match make_parser awkish_grammar accept_all[small_frag] with
    | Some tree -> parse_tree_leaves tree = small_frag
    | _ -> false