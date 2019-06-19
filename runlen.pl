
decode_rl([], []).

decode_rl([(A,0)|Tail], L) :- !,
	decode_rl(Tail, L).
	

decode_rl([(A, B)|Tail], [A|L]) :- !,
	B1 is B - 1,
	decode_rl([(A, B1)|Tail], L).

decode_rl([X|Tail], [X|L]) :-
	decode_rl(Tail, L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_rl([], []).

encode_rl([H|T], [Count|ResT]) :-
	count([H|T], Rem, 1, Count),
	encode_rl(Rem, ResT).



count([X|[Y|Tail]], Rem, Count, (X, R)) :-
	X == Y,	
	C is Count + 1,
	count([X|Tail], Rem, C, (X, R)), !.

count([X|Tail], Tail, Count, (X,Count)) :-
	Count > 1, !.
	

count([X|Tail], Tail, Count, X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 

?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L).

	L = [p(3), p(X), q(X), q(Y), q(4)]

	Dinetai i parapano apantisi epeidi exoume X==Y kai oxi X=Y, 
	epomenos den ginetai enopoiisi tou X me kapoion arithmo alla apli sygkrisi.
	An eixame X = Y tha pairname san apotelesma L = [(p(3), 2), (q(3), 2), q(4)].


*/










