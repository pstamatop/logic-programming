:- set_flag(print_depth, 1000).

liars(L, Res) :-
	count(L, Count),
	liar(L, Res, -1, Count).



/* Generate N lists of liars (0 <= N <= Number of people in the group) */
liar(Statements, R, Liars, Count) :-
	Liars < Count,
	N is Liars + 1,
	check(Statements, N, R).

liar(Statements, R, Liars, Count) :-
	Liars < Count,
	N is Liars + 1,
	liar(Statements, R, N, Count).



count([], 0).

count([X|Y], Res) :-
	count(Y, R),
	Res is R + 1.



countLiars([], 0).

countLiars([X|Y], Res) :-
	countLiars(Y, R),
	Res is R + X.



/* Construct list of liars based on supposed number of liars (Count). 
   The list will be checked afterwards */
buildList([], _, []).

buildList([X|Y], Count, [0|L]) :-
	X =< Count,
	buildList(Y, Count, L), !.

buildList([X|Y], Count, [1|L]) :-
	buildList(Y, Count, L), !.



/* Construct list of liars and check its validity */
check(Statements, Count, R) :-
	buildList(Statements, Count, List),
	countLiars(List, C),
	C == Count,
	append([], List, R).
