:- set_flag(print_depth, 10000).

dominos([(0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (1, 1), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (2, 2), (2, 3), (2, 4), (2, 5), (2, 6), (3, 3), (3, 4), (3, 5), (3, 6), (4, 4), (4, 5), (4, 6), (5, 5), (5, 6), (6, 6)]).

frame([[3, 1, 2, 6, 6, 1, 2, 2], [3, 4, 1, 5, 3, 0, 3, 6], [5, 6, 6, 1, 2, 4, 5, 0], [5, 6, 4, 1, 3, 3, 0, 0], [6, 1, 0, 6, 3, 2, 4, 0], [4, 1, 5, 2, 4, 3, 5, 5], [4, 1, 0, 2, 4, 5, 2, 0]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

put_dominos :-
	dominos(Dominos),
	frame(Frame),
	get_all_positions(Dominos, Frame, Positions),
	quick_sort(Positions, Sorted),
	place_dominos(Sorted, Result),
	print_all(Result, Frame).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_values((X, Y), X, Y).

get_all_positions([], _, []).

get_all_positions([X|Y], Frame, [Res|L]) :-
	possible_positions(X, Frame, Res),
	get_all_positions(Y, Frame, L).

possible_positions(X, Frame, L) :-
	get_values(X, A, B),
	append([], [X], Tile),
	check_rows(Frame, 1, A, B, L1),
	flatten_result(L1, F1),
	check_columns(Frame, 1, A, B, L2),
	flatten_result(L2, F2),
	append(F1, F2, F),
	append(Tile, F, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% http://kti.ms.mff.cuni.cz/~bartak/prolog/sorting.html

quick_sort(Positions,Sorted):-
	q_sort(Positions,[],Sorted).

q_sort([],Acc,Acc).
	
q_sort([H|T],Acc,Sorted):-
	pivoting(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),
	q_sort(L2,[H|Sorted1],Sorted).

pivoting(_,[],[],[]).
pivoting(H,[X|T],[X|L],G):-
	length(X, L1),
	length(H, L2),
	L1 > L2,
	pivoting(H,T,L,G).
pivoting(H,[X|T],L,[X|G]):-
	length(X, L1),
	length(H, L2),
	L1 =< L2,
	pivoting(H,T,L,G).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

place_dominos([], []).

place_dominos([[Tile, Pos|_]|Lists], [Res|Result]) :-
	append([], Tile, L),
	append([L], [Pos], Res),
	delete_slots(Lists, Pos, Slots),
	quick_sort(Slots, Updated),
	place_dominos(Updated, Result).

place_dominos([[Tile, _|Positions]|Lists], Result) :-
	place_dominos([[Tile|Positions]|Lists], Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

delete_slots([], _, []).

delete_slots([[(A,B)|X]|Y], [(R1, C1),(R2, C2)], [R|Res]) :-
	update_list(X, [(R1, C1),(R2, C2)], New),
	append([(A,B)], New, R),
	delete_slots(Y, [(R1, C1),(R2, C2)], Res).

update_list([], _, []).

update_list([[(R1, C1),(R2, C2)]|Y], [(Row1, Column1),(Row2, Column2)], [[(R1, C1),(R2, C2)]|Res]) :-
	(R1, C1) \== (Row1, Column1),
	(R1, C1) \== (Row2, Column2),
	(R2, C2) \== (Row1, Column1),
	(R2, C2) \== (Row2, Column2),
	update_list(Y, [(Row1, Column1),(Row2, Column2)], Res), !.

update_list([[(_, _),(_, _)]|Y], [(Row1, Column1),(Row2, Column2)], Res) :-
	update_list(Y, [(Row1, Column1),(Row2, Column2)], Res), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check possible positions in each row
check_rows([], _, _, _, []).
check_rows([H|T], Index, X, Y, [Res|L]) :-
	check_row(Index, 1, H, X, Y, Res),
	I is Index + 1,
	check_rows(T, I, X, Y, L).

check_row(_, _, [_], _, _, []).
check_row(Row, Column, [H1|[H2|T]], X, Y, [[(Row, Column),(Row, Column1)]|Res]) :-
	H1 == X,
	H2 == Y,
	Column1 is Column + 1,
	check_row(Row, Column1, [H2|T], X, Y, Res), !.
check_row(Row, Column, [H1|[H2|T]], X, Y, [[(Row, Column1),(Row, Column)]|Res]) :-
	H1 == Y,
	H2 == X,
	X \= Y,
	Column1 is Column + 1,
	check_row(Row, Column1, [H2|T], X, Y, Res), !.
check_row(Row, Column, [_|[H2|T]], X, Y, Res) :-
	Column1 is Column + 1,
	check_row(Row, Column1, [H2|T], X, Y, Res), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Check possible positions in each column 
check_columns([_], _, _, _, []) :- !.
check_columns([H1|[H2|T]], Index, X, Y, [Res|L]) :-
	check_column(Index, 1, H1, H2, X, Y, Res),
	I is Index + 1,
	check_columns([H2|T], I, X, Y, L).

check_column(_, _, [], [], _, _, []).
check_column(Row, Column, [X1|Y1], [X2|Y2], X, Y, [[(Row, Column),(Row1, Column)]|Res]) :-
	X1 == X,
	X2 == Y,
	Column1 is Column + 1,
	Row1 is Row + 1,
	check_column(Row, Column1, Y1, Y2, X, Y, Res), !.
check_column(Row, Column, [X1|Y1], [X2|Y2], X, Y, [[(Row1, Column),(Row, Column)]|Res]) :-
	X1 == Y,
	X2 == X,
	X \= Y,
	Column1 is Column + 1,
	Row1 is Row + 1,
	check_column(Row, Column1, Y1, Y2, X, Y, Res), !.
check_column(Row, Column, [_|Y1], [_|Y2], X, Y, Res) :-
	Column1 is Column + 1,
	check_column(Row, Column1, Y1, Y2, X, Y, Res), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Fix list of possible positions' coordinates
flatten_result([], []) :- !.

flatten_result([H|T], F):- !,
	flatten_result(T, FT),
	append(H, FT, F).

flatten_result(X, [X]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_all(Positions, [H|Frame]) :-
	length([H|Frame], Rows),
	length(H, Columns),
	print(Positions, 1, Rows, Columns).

print(Positions, Row, Rows, Columns) :-
	Row =< Rows,
	frame_row(Row, 1 ,Columns, Positions),
	frame_column(Row, 1, Columns, Positions),
	R is Row + 1,
	print(Positions, R, Rows, Columns).
	
print(Positions, Row, Rows, Columns) :-
	Row == Rows.

frame_row(Row, Column, MaxC, Positions) :-
	Column > MaxC, nl.

frame_row(Row, Column, MaxC, Positions) :-
	Column =< MaxC, 
	search_positionsR(Positions, Row, Column, Number, Next),
	Next == 1,
	write(Number), write('-'),
	C is Column + 1,
	frame_row(Row, C, MaxC, Positions), !.

frame_row(Row, Column, MaxC, Positions) :-
	Column =< MaxC, 
	search_positionsR(Positions, Row, Column, Number, Next),
	Next == 0,
	write(Number), write(' '),
	C is Column + 1,
	frame_row(Row, C, MaxC, Positions), !.


frame_column(Row, Column, MaxC, Positions) :-
	Column > MaxC, nl.

frame_column(Row, Column, MaxC, Positions) :-
	Column =< MaxC, 
	search_positionsC(Positions, Row, Column, Number, Down),
	Down == 1,
	write('| '),
	C is Column + 1,
	frame_column(Row, C, MaxC, Positions), !.

frame_column(Row, Column, MaxC, Positions) :-
	Column =< MaxC, 
	search_positionsC(Positions, Row, Column, Number, Down),
	Down == 0,
	write('  '),
	C is Column + 1,
	frame_column(Row, C, MaxC, Positions), !.

%% 'Next' is boolean to check for adjacent number
search_positionsR([[(A, _), [(X1, Y1), (X2, Y2)]]|_], Row, Column, Number, Next) :- 
	X1 == Row, Y1 == Column,
	Number = A,
	X1 == X2,
	Y2 > Y1,
	Next is 1.

search_positionsR([[(A, _), [(X1, Y1), (_, _)]]|_], Row, Column, Number, Next) :- 
	X1 == Row, Y1 == Column,
	Number = A,
	Next is 0.

search_positionsR([[(_, B), [(X1, Y1), (X2, Y2)]]|_], Row, Column, Number, Next) :-  
	X2 == Row, Y2 == Column,
	Number = B,
	X1 == X2,
	Y1 > Y2,
	Next is 1.

search_positionsR([[(_, B), [(_, _), (X2, Y2)]]|_], Row, Column, Number, Next) :-  
	X2 == Row, Y2 == Column,
	Number = B,
	Next is 0.

search_positionsR([H|T], Row, Column, Number, Next) :-
	search_positionsR(T, Row, Column, Number, Next), !.


%% 'Down' is boolean to check for adjacent number
search_positionsC([[(A, _), [(X1, Y1), (X2, Y2)]]|_], Row, Column, Number, Down) :- 
	X1 == Row, Y1 == Column,
	Number = A,
	Y1 == Y2,
	X2 > X1,
	Down is 1.

search_positionsC([[(A, _), [(X1, Y1), (_, _)]]|_], Row, Column, Number, Down) :- 
	X1 == Row, Y1 == Column,
	Number = A,
	Down is 0.

search_positionsC([[(_, B), [(X1, Y1), (X2, Y2)]]|_], Row, Column, Number, Down) :-  
	X2 == Row, Y2 == Column,
	Number = B,
	Y1 == Y2,
	X1 > X2,
	Down is 1.

search_positionsC([[(_, B), [(_, _), (X2, Y2)]]|_], Row, Column, Number, Down) :-  
	X2 == Row, Y2 == Column,
	Number = B,
	Down is 0.

search_positionsC([H|T], Row, Column, Number, Down) :-
	search_positionsC(T, Row, Column, Number, Down), !.
