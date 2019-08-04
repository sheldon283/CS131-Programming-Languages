(* Symbol type declaration *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* parse_tree type declaration*)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

let rec rule_conversion nonterminal firstRule = 
    match firstRule with 
    | [] -> []
    | (current, right) :: connected -> 
        if current != nonterminal
        then rule_conversion nonterminal connected
        else right::(rule_conversion nonterminal connected)
;;

(* convert_grammar: returns a Homework 2-style grammar, which is converted from the Homework 1-style grammar gram1 *)
let convert_grammar gram1 = 
    let firstComponent = Pervasives.fst gram1 in
    let secondComponent = Pervasives.snd gram1 in
    (firstComponent, fun nonterminal -> (rule_conversion nonterminal secondComponent))

(* parse_tree_leaves:  traverses the parse tree tree left to right and yields a list of the leaves encountered. *)
let rec parse_tree_leaves tree = 
    match tree with 
        | Node (node_val, subtree) -> parse_subtree_list subtree
        | Leaf leaf_val -> [leaf_val]

and parse_subtree_list stl = 
    match stl with
        | [] -> []
        | car :: cdr -> 
        let parse_leaves = parse_tree_leaves car in
        let parse_subtree_rec = parse_subtree_list cdr in
        List.append parse_leaves parse_subtree_rec

(* Adapted from sample solution *)
let match_empty accept frag = accept frag

let match_nothing accept frag = None

let match_terminal terminal accept frag =
    match frag with
        | [] -> None
        | car :: cdr -> if car = terminal then accept cdr else None

let rec make_list_matcher rules pf_snd =
    match rules with
        | [] -> match_nothing
        | car :: cdr -> (fun accept frag ->
            let tail_matcher = make_list_matcher cdr pf_snd in 
            let total_head_matcher = (make_given_matcher car pf_snd) accept frag in
                match total_head_matcher with
                    | None -> tail_matcher accept frag
                    | x -> x)

and make_given_matcher symbol pf_snd =
    match symbol with
        | [] -> match_empty
        | (N car) :: cdr -> (fun accept frag -> 
            (make_list_matcher (pf_snd car) pf_snd) ((make_given_matcher cdr pf_snd) accept) frag)
        | (T car) :: cdr -> (fun accept frag -> let new_make_and = (make_given_matcher cdr pf_snd accept) in
            match_terminal car new_make_and frag)

(* make_matcher: returns a matcher for the grammar gram *)
let make_matcher gram =
	fun accept frag -> make_list_matcher ((Pervasives.snd gram) (Pervasives.fst gram)) (Pervasives.snd gram) accept frag


(* Doesn't work *)
let parser_match_empty accept frag = accept frag

let parser_match_nothing accept frag = None

let parser_match_terminal terminal accept frag =
    match frag with
        | [] -> None
        | car :: cdr -> if car = terminal then accept cdr else None

let rec parser_make_alternative_matcher rules pf_snd =
    match rules with
        | [] -> parser_match_nothing
        | car :: cdr -> (fun accept frag ->
            let tail_matcher = parser_make_alternative_matcher cdr pf_snd in 
            let total_head_matcher = (parser_make_symbol_matcher car pf_snd) accept frag in
                match total_head_matcher with
                    | None -> tail_matcher accept frag
                    | x -> x)

and parser_make_symbol_matcher symbol pf_snd =
    match symbol with
        | [] -> parser_match_empty
        | (N car) :: cdr -> (fun accept frag -> 
            (parser_make_alternative_matcher (pf_snd car) pf_snd) ((parser_make_symbol_matcher cdr pf_snd) accept) frag)
        | (T car) :: cdr -> (fun accept frag -> let new_make_and = (parser_make_symbol_matcher cdr pf_snd accept) in
            parser_match_terminal car new_make_and frag)

(* make_matcher: returns a matcher for the grammar gram *)
let make_parser gram =
	Some gram

