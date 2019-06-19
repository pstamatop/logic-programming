:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).

tents(RowTents, ColumnTents, Trees, Tents) :-
	length(RowTents, M),
	length(ColumnTents, N),
	Full is N * M,
	length(Tents1, Full),
	length(Tents2, Full),
	Tents1 #:: 0..1,
	Tents2 #:: 0..1,
	constrain(RowTents, ColumnTents, Trees, Tents1, M, N),
	constrain(RowTents, ColumnTents, Trees, Tents2, M, N),
	Cost #= sum(Tents1),
	Cost #= sum(Tents2),
	bb_min(search(Tents1, 0, first_fail, indomain_split, complete, []), Cost, bb_options{strategy:restart}),
	search(Tents2, 0, first_fail, indomain_split, complete, []),
	print(Tents2, N, 0, Tents).

constrain(RowTents, ColumnTents, Trees, Tents, M, N) :-
	row_con(RowTents, Tents, N, 1),
	column_con(ColumnTents, Tents, M, N, 1),
	same_spot(Trees, Tents, 1, N),
	tent_tree(Trees, Tents, M, N),
	nonconsecutive_tents(Tents, Tents, 1, M, N).

row_con([], _, _, _) :- !.
row_con([RowTents|Rest], Tents, N, I) :-
	RowTents < 0, !,
	I1 is I + 1,
	row_con(Rest, Tents, N, I1).
row_con([RowTents|Rest], Tents, N, I) :-
	get_indices_row(I, N, 1, L),
	check_tents(Tents, L, 1, Res),
	sum(Res) #=< RowTents,
	I1 is I + 1,
	row_con(Rest, Tents, N, I1).

column_con([], _, _, _, _) :- !.
column_con([ColumnTents|Rest], Tents, M, N, J) :-
	ColumnTents < 0, !,
	J1 is J + 1,
	column_con(Rest, Tents, M, N, J1).
column_con([ColumnTents|Rest], Tents, M, N, J) :-
	get_indices_column(J, M, N, 0, L),
	check_tents(Tents, L, 1, Res),
	sum(Res) #=< ColumnTents,
	J1 is J + 1,
	column_con(Rest, Tents, M, N, J1).

tent_tree([], _, _, _).
tent_tree([I - J|Rest], Tents, M, N) :-
	Index is (I-1) * N + J,
	adjacent(Index, M, N, Neighbors),
	check_tents(Tents, Neighbors, 1, Res),
	sum(Res) #> 0,
	tent_tree(Rest, Tents, M, N).


same_spot([], _, _, _).
same_spot([I - J|Rest], [_|Tents], T, N) :-
	Index is (I-1) * N + J,
	Index =\= T, !,
	T1 is T + 1,
	same_spot([I - J|Rest], Tents, T1, N).
same_spot([_ - _|Rest], [Tent|Tents], T, N) :-
	Tent #= 0,
	T1 is T + 1,
	same_spot(Rest, Tents, T1, N).


nonconsecutive_tents([], _, _, _, _).
nonconsecutive_tents([Tent|Rest], Tents, T, M, N) :-
	adjacent(T, M, N, Neighbors),
	check_tents(Tents, Neighbors, 1, Res),
	Tent #= 1 => sum(Res) #= 0,
	T1 is T + 1,
	nonconsecutive_tents(Rest, Tents, T1, M, N).

%Returns list of neighbors with ascending order of indices
adjacent(T, _, N, [R, B, BR]) :- %% top left
	T =:= 1, !,
	R is T + 1, B is T + N, BR is N + 2.
adjacent(T, _, N, [L, BL, B]) :- %% top right
	T =:= N, !,
	L is N - 1, B is 2 * N, BL is 2 * N - 1.
adjacent(T, M, N, [U, UR, R]) :- %% bottom left
	T =:= (M-1) * N + 1, !,
	U is (M-2) * N + 1, UR is (M-2) * N + 2, R is (M-1) * N + 2.
adjacent(T, M, N, [UL, U, L]) :- %% bottom right
	T =:= N * M, !,
	U is (M-1) * N, UL is (M-1) * N - 1, L is N * M - 1.
adjacent(T, _, N, [U, UR, R, B, BR]) :- %% first column
	mod(T, N, Mod),
	Mod =:= 1, !,
	U is T - N, UR is T - N + 1, R is T + 1, B is T + N, BR is T + N + 1.
adjacent(T, _, N, [UL, U, L, BL, B]) :- %% last column
	mod(T, N, Mod),
	Mod =:= 0, !,
	U is T - N, UL is T - N - 1, L is T - 1, BL is T + N - 1, B is T + N.
adjacent(T, _, N, [L, R, BL, B, BR]) :- %% first row
	T =< N, !,
	L is T - 1, R is T + 1, B is T + N, BL is T + N - 1, BR is T + N + 1.
adjacent(T, M, N, [UL, U, UR, L, R]) :- %% last row
	T >= (M-1) * N + 1, !,
	L is T - 1, R is T + 1, U is T - N, UL is T - N - 1, UR is T - N + 1.
adjacent(T, _, N, [UL, U, UR, L, R, BL, B, BR]) :- !, %% middle cells
	UL is T - N - 1, U is T - N, UR is T - N + 1,
	L is T - 1, R is T + 1,
	BL is T + N - 1, B is T + N, BR is T + N + 1.

check_tents(_, [], _, []).
check_tents([_|Tents], [N|Neighbors], I, Res) :-
	I < N, !,
	Next is I + 1,
	check_tents(Tents, [N|Neighbors], Next, Res).
check_tents([T|Tents], [_|Neighbors], I, [T|Res]) :-
	Next is I + 1,
	check_tents(Tents, Neighbors, Next, Res).

get_indices_row(_, N, M, []) :-
	M > N, !.
get_indices_row(Row, N, M, [X|L]) :-
	X is (Row-1) * N + M,
	M1 is M + 1,
	get_indices_row(Row, N, M1, L).

get_indices_column(_, M, _, X, []) :-
	X =:= M, !.
get_indices_column(Column, M, N, J, [X|L]) :-
	X is (J*N) + Column,
	J1 is J + 1,
	get_indices_column(Column, M, N, J1, L).

print([], _, _, []).
print([1|Tents], N, T, [Row - Column|List]) :-
	div(T, N, R),
	mod(T, N, C),	
	Row is R + 1,
	Column is C + 1,
	T1 is T + 1,
	print(Tents, N, T1, List).
print([0|Tents], N, T, List) :-
	T1 is T + 1,
	print(Tents, N, T1, List).
