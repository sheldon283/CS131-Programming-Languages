let subset_test0 = subset [] []
let subset_test1 = subset [] [1; 3; 4]
let subset_test2 = subset [2;4;2] [1;2;4]
let subset_test3 = not (subset [1;2;5] [6;1;3])

let my_equal_sets_test0 = equal_sets [1;3;4] [4;3;1]
let my_equal_sets_test1 = not (equal_sets [5] [])

let my_set_union_test0 = equal_sets (set_union [1] [4]) [1;4]
let my_set_union_test1 = equal_sets (set_union ["abc"; "seg"] ["seg"; "new"]) ["seg"; "new"; "abc"]
let my_set_union_test2 = equal_sets (set_union [1;1] [4;4;4]) [4;1]

let my_set_intersection_test0 = equal_sets (set_intersection [23;11;462] [1;2]) [] 
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3;4;5;6;7;8] [2;4;6;9]) [2;4;6]
let my_set_intersection_test2 = equal_sets (set_intersection [2;2;2;4;4;4;4;4] [4]) [4]

let my_set_diff_test0 = equal_sets (set_diff [1;4;5;6;2;7;8;] [1;2;3;4]) [5;6;7;8]
let my_set_diff_test1 = equal_sets (set_diff [4] [1;2;3;4]) []

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x +. 0.) 1023. = 1023.
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x / 3) 132 = 0

type non_terminals =
  | Topic | Noun | Verb | Adjective | Sentence

let grammar_test0 =
  Topic,
  [
    Adjective, [T"brown"];
    Noun, [T"dog"];
    Verb, [T"jump"];
    Sentence, [N Adjective];
    Sentence, [N Noun];
    Sentence, [N Verb];
    Topic, [N Noun];
    Topic, [N Sentence; T","; N Topic]]

let my_filter_reachable_test0 =
  filter_reachable grammar_test0 = grammar_test0

let my_filter_reachable_test1 = 
    filter_reachable (Noun, snd grammar_test0) = (Noun, [Noun, [T"dog"]])