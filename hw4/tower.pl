col_length([], _).
col_length([H | T], N) :- 
	length(H, N),
	col_length(T, N).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

%==============================================================
% tower:

less_check(_, []).
less_check(Val, [Head | Tail]) :-
	Val #> Head,
	less_check(Val, Tail).

reverse_all_lsts([], []).
reverse_all_lsts([XRev | LRev], [X | L]) :-
	reverse(X, XRev),
	reverse_all_lsts(LRev, L).

verify_rc_helper([], []).
verify_rc_helper([Val | OtherVals], [Head | Tail]) :-
    verification(Head, Count, []), 
    Count = Val,
	verify_rc_helper(OtherVals, Tail).

verification([], X, _) :- X = 0.
verification([Head | Tail], Count, BE) :-
	append(BE, [Head], NewBE),
	verification(Tail, C1, NewBE),
	(check_all_less(Head, BE) -> Count is C1 + 1; C1 = Count).

check_all_less(Head, BE) :-
    less_check(Head, BE).

verify_lr(T, Left, Right) :-
    verify_left(T, Left),
    reverse_all_lsts(TR, T), 
    verify_right(TR, Right).

verify_left(T, Left) :-
    verify_rc_helper(Left, T).

verify_right(TR, Right) :-
    verify_rc_helper(Right, TR).

verify_tb(A, Top, Bottom) :-
    verify_top(A, Top),
    reverse_all_lsts(AR, A), 
    verify_bottom(AR, Bottom).

verify_top(A, Top) :-
    verify_rc_helper(Top, A).

verify_bottom(AR, Bottom) :-
    verify_rc_helper(Bottom, AR).

row_fd(N, L) :-
    length(L, N),
    fd_all_different(L),
	fd_domain(L, 1, N).

col_fd(N, L) :-
    length(L, N),
    fd_all_different(L).

check_count_fd(N, C) :- 
    length(C, N),
    fd_domain(C, 1, N).

tower(N, T, C) :-
	C = counts(Top, Bottom, Left, Right),
	length(T, N),
	col_length(T, N),
	maplist(row_fd(N), T),
	transpose(T, TM),
	maplist(col_fd(N), TM),
    maplist(fd_labeling, T),
    maplist(check_count_fd(N), [Top, Bottom, Left, Right]),
    verify_lr(T, Left, Right),
    verify_tb(TM, Top, Bottom).

%==============================================================
% plain_tower:

make_list(N, L) :- 
    findall(Val, between(1, N, Val), L).

check_count_plain(N, C) :- 
    length(C, N).							

check_permutation(Lst, L) :- 
    permutation(Lst, L).

make_domain(N, L) :-
    make_list(N, X), 
    check_permutation(X, L).

count_plain([],[]).      
count_plain([H|T], [TH|TR]) :-
    plain_tower_count(0, H, TH),
    count_plain(T, TR).

reverse_all_lsts([], []).
reverse_all_lsts([XRev | LRev], [X | L]) :-
	reverse(X, XRev),
	reverse_all_lsts(LRev, L).

plain_tower_count(_, [], 0).    
plain_tower_count(THead, [Head|Tail], Top) :- 
    Head > THead, 
    plain_tower_count(Head, Tail, Count),
    Top is Count + 1.
plain_tower_count(THead, [Head|Tail], Top) :-
    Head < THead, 
    plain_tower_count(THead, Tail, Top).
    
count_and_rotate(A, B, Dir1, Dir2) :-
    count_plain(A, Dir1),
    transpose(B, C),
    reverse_all_lsts(Dir2, C).

two_reverse(A, B, C, D) :-
    reverse(A, B),
    reverse(C, D).

plain_tower(N, T, C) :-
    C = counts(Top, Bottom, Left, Right),    
    length(T, N),  
    maplist(check_count_plain(N), [Top, Bottom, Left, Right]),
    col_length(T, N),  
    make_list(N, Lst),
    maplist(check_permutation(Lst), T), 
    count_and_rotate(T, T, Left, TDown),
    count_and_rotate(TDown, TDown, Bottom, TRight),
    count_and_rotate(TRight, TRight, RRight, TUp),
    count_plain(TUp, RTop),
    two_reverse(Right, RRight, Top, RTop),
    maplist(make_domain(N), TDown).

%===================================================
%stats:

tower_time(T_time) :-
    statistics(cpu_time,[Time1|_]),
    tower(5, T, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
    statistics(cpu_time,[Time2|_]),
    T_time is Time2 - Time1.

plain_time(P_time) :-
    statistics(cpu_time,[Time1|_]),
    plain_tower(5, T, counts([2,3,2,1,4], [3,1,3,3,2], [4,1,2,5,2], [2,4,2,1,2])),
    statistics(cpu_time,[Time2|_]),
    P_time is Time2 - Time1.

speedup(N) :- 
	tower_time(T_time), plain_time(P_time), 
	N is P_time / T_time.

%=========================================
%ambiguous:

ambiguous(N, C, T1, T2) :-
	tower(N, T1, C),
	tower(N, T2, C),
	T1 \= T2.