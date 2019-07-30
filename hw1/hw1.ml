type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
;;

type awksub_nonterminals = | Expr | Lvalue | Incrop | Binop | Num ;;

let rec subset a b = 
  match a with
    | [] -> true
    | car :: cdr -> 
      if List.exists ((=) car) b 
      then subset cdr b 
      else false
;;

let rec equal_sets a b = 
  subset a b && subset b a
;;

let rec set_union a b = 
  match a with
    | [] -> b
    | car :: cdr -> 
      if List.exists ((=) car) b 
      then set_union cdr b 
      else set_union cdr (car :: b)
;;

let rec set_intersection a b = 
  match a with
    | [] -> []
    | car :: cdr -> 
      if List.exists ((=) car) b 
      then car :: (set_intersection cdr b) 
      else set_intersection cdr b
;;

let rec set_diff a b = 
  List.filter (fun x -> not (List.exists ((=) x) b )) a
;;

let rec computed_fixed_point eq f x = 
  if eq x (f x)
  then x
  else computed_fixed_point eq f (f x)
;;

let rec nonterminals_filter rules = 
	match rules with
    | [] -> []
    | N car :: cdr -> car :: nonterminals_filter cdr
    | T car :: cdr -> nonterminals_filter cdr
;;

let rec reachables n rules = 
  match rules with
    | [] -> []
    | (l , r) :: cdr -> 
      if l != n 
      then reachables n cdr
      else set_union (nonterminals_filter r) (reachables n cdr)
;;

let rec process_nonterminal_collections n rules = 
  match n with
    | [] -> []
    | car :: cdr -> set_union (set_union [car] (reachables car rules)) (process_nonterminal_collections cdr rules)
;;

let rec process_nonterminal n rules =
  set_union n (process_nonterminal_collections n rules)
;;

let rec filter_reachable g = 
	(Pervasives.fst g), (List.filter (fun (l, r) -> 
    List.exists ((=) l) (computed_fixed_point equal_sets (fun e -> (process_nonterminal e (Pervasives.snd g))) [Pervasives.fst g] )) (Pervasives.snd g))
;;