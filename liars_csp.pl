:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(ic_global).

genrand(N, List) :-
   length(List, N),
   make_list(N, List).
make_list(_, []).
make_list(N, [X|List]) :-
   random(R),
   X is R mod (N+1),
   make_list(N, List).

liars_csp(Statements, Res) :-
	length(Statements, Count),
	length(Res, Count),	
	Res #:: 0..1,
	Sum #= sum(Res),
	constrain(Statements, Sum, Res),
	search(Res, 0, first_fail, indomain, complete, []).

constrain([], _, []).
constrain([S|Statements], Sum, [R|Res]) :-
	R #= (S #> Sum),
	constrain(Statements, Sum, Res).



