:- lib(ic).
:- lib(ic_global).
:- lib(branch_and_bound).
:- set_flag(print_depth, 10000).

vehicles([35, 40, 55, 15, 45, 25, 85, 55]).

clients([c(15,  77,  97), c(23, -28,  64), c(14,  77, -39), c(13,  32,  33), c(18,  32,   8), c(18, -42,  92), c(19,  -8,  -3), 
		 c(10,   7,  14), c(18,  82, -17), c(20, -48, -13), c(15,  53,  82), c(19,  39, -27), c(17, -48, -13), c(12,  53,  82), 
		 c(11,  39, -27), c(15, -48, -13), c(25,  53,  82), c(14, -39,   7), c(22,  17,   8), c(23, -38,  -7)]).


hcvrp(NCl, NVe, Timeout, Solution, Cost, Time) :-
	Timeout =:= 0, !,
	Full is NCl * NVe,
	length(Result, Full), %% flattened list
	Result #:: 0..NCl,
	clients(AllClients),
	vehicles(AllVehicles),
	getN(NCl, AllClients, Clients),
	getN(NVe, AllVehicles, Vehicles),
	getValuesLists(Clients, Quantities, Xs, Ys),
	distances(Xs, Ys, NCl, Distances),  %% construct distance matrix
	constraints(Result, Quantities, Vehicles, NVe, NCl, Distances),
	costDef(Result, NVe, NCl, Distances, Costs),
	flatten(Costs, FlatCosts),
	Cost #= sum(FlatCosts),
	bb_min(search(Result, 0, first_fail, indomain_split, complete, []), Cost, _),
	unflatten(Result, NCl, Sol), %% convert to list of lists
	deleteAllZeros(Sol, Solution).

hcvrp(NCl, NVe, Timeout, Solution, Cost, Time) :-
	Timeout =\= 0, !,
	Full is NCl * NVe,
	length(Result, Full), %% flattened list
	Result #:: 0..NCl,
	clients(AllClients),
	vehicles(AllVehicles),
	getN(NCl, AllClients, Clients),
	getN(NVe, AllVehicles, Vehicles),
	getValuesLists(Clients, Quantities, Xs, Ys),
	distances(Xs, Ys, NCl, Distances),  %% construct distance matrix
	constraints(Result, Quantities, Vehicles, NVe, NCl),
	costDef(Result, NVe, NCl, Distances, Costs),
	flatten(Costs, FlatCosts),
	Cost #= sum(FlatCosts),
	bb_min(search(Result, 0, first_fail, indomain_split, complete, []), Cost, bb_options{timeout:Timeout}),
	unflatten(Result, NCl, Sol), %% convert to list of lists
	deleteAllZeros(Sol, Solution).


constraints(Result, Quantities, Vehicles, NVe, NCl) :-
	capacities(Result, 1, NVe, NCl, Quantities, Vehicles),
	count_occurrences(Result, NCl, 0).

count_occurrences(_, NCl, NCl).
count_occurrences(Result, NCl, Client) :-
	Cl is Client + 1,
	occurrences(Cl, Result, 1),
	count_occurrences(Result, NCl, Cl).

%% For each vehicle make sure that orders <= capacity
capacities([], _, _, _, _, _).
capacities(Result, Vehicle, NVe, NCl, Quantities, Vehicles) :-
	getRow(Result, Remain, Row, NCl, 1),
	element(Vehicle, Vehicles, Capacity),
	checkRow(Row, Quantities, Res),
	sum(Res) #=< Capacity,
	Next is Vehicle + 1,
	capacities(Remain, Next, NVe, NCl, Quantities, Vehicles).

costDef([], _, _, _, []).
costDef(Result, NVe, NCl, Distances, [Cs|Costs]) :-
	getRow(Result, Remain, Row, NCl, 1),
	consecutive(Row, NCl, Indices),	%% Returns list of indices for distance matrix
	costPerRow(Indices, Distances, Cs),
	costDef(Remain, NVe, NCl, Distances, Costs), !.

%% Returns list of distances per vehicle
costPerRow([], _, []).
costPerRow([I|Indices], Distances, [C|Costs]) :-
	element(I, Distances, C),
	costPerRow(Indices, Distances, Costs).

%% Returns orders of row's clients
checkRow([], _, []).
checkRow([C|Clients], Quantities, [Q|Result]) :-
	C #> 0, !,
	element(C, Quantities, Q),
	checkRow(Clients, Quantities, Result).
checkRow([C|Clients], Quantities, [0|Result]) :-
	C #= 0,
	checkRow(Clients, Quantities, Result).

%% Returns I-th row of flattened list
getRow([L|List], List, [L|[]], NCl, NCl).
getRow([L|List], Remain, [L|Row], NCl, Index) :-
	I is Index + 1,
	getRow(List, Remain, Row, NCl, I), !.

%% Returns first N elements from list
getN(N, Full, Partial) :-
	length(Partial, N),
	append(Partial, _, Full).

getValues(c(D, X, Y), D, X, Y).

getValuesLists([], [], [], []).
getValuesLists([H|Clients], [Q|Quantities], [X|Xs], [Y|Ys]) :-
	getValues(H, Q, X, Y),
	getValuesLists(Clients, Quantities, Xs, Ys).

%% Euclidean distance of two points
distance(Xs, Ys, Index1, Index2, Distance) :-
	Index1 =\= 0, Index2 =\= 0, !,
	element(Index1, Xs, X1), element(Index2, Xs, X2),
	element(Index1, Ys, Y1), element(Index2, Ys, Y2),
    Z is (X2-X1)*(X2-X1) + (Y2-Y1)*(Y2-Y1),
    sqrt(Z, D),
    Dis is round(D * 1000),
    Distance is integer(Dis).
distance(Xs, Ys, Index1, Index2, Distance) :-
	Index1 =\= 0, Index2 =:= 0, !,
	element(Index1, Xs, X1),
	element(Index1, Ys, Y1),
	Z is X1*X1 + Y1*Y1,
    sqrt(Z, D),
    Dis is round(D * 1000),
    Distance is integer(Dis).
distance(Xs, Ys, Index1, Index2, Distance) :-
	Index1 =:= 0, Index2 =\= 0, !,
	element(Index2, Xs, X2),
	element(Index2, Ys, Y2),
	Z is X2*X2 + Y2*Y2,
    sqrt(Z, D),
    Dis is round(D * 1000),
    Distance is integer(Dis).
distance(_, _, _, _, 0).

%% Construct (NCl+1)*(NCl+1) matrix
calcDistances(_, _, NCl, Client1, _, []) :-
	Client1 > NCl, !.
calcDistances(Xs, Ys, NCl, Client1, Client2, Distances) :-
	Client2 > NCl, !,
	NextClient1 is Client1 + 1,
	NextClient2 is 0,
	calcDistances(Xs, Ys, NCl, NextClient1, NextClient2, Distances).
calcDistances(Xs, Ys, NCl, Client1, Client2, [D|Distances]) :-
	distance(Xs, Ys, Client1, Client2, D), !,
	NextClient2 is Client2 + 1,
	calcDistances(Xs, Ys, NCl, Client1, NextClient2, Distances).

%% Calculate distance between all points
distances(Xs, Ys, NCl, Distances) :-
	calcDistances(Xs, Ys, NCl, 0, 0, Distances).


%% Input: list of clients to be served,
%% Output: indices of distance matrix
consecutive(Clients, NCl, Result) :-
	pairs(Clients, NCl, 0, Result).

pairs([], NCl, Previous, [Index|Result]) :-
	Previous =\= 0,
	Index is (Previous*NCl+1) + 1,
	pairs([], NCl, 0, Result).
pairs([], _, Previous, []) :-
	Previous =:= 0.
pairs([C], NCl, Previous, [Index|Result]) :-
	Index is Previous*(NCl+1) + C + 1,
	pairs([], NCl, C, Result).
pairs([C|[Next|Clients]], NCl, Previous, [Index|Result]) :-
	Index is Previous*(NCl+1) + C + 1,
	pairs([Next|Clients], NCl, C, Result), !.

%% Printing Purposes
%% Convert list to list of lists, N is the length of each sublist
unflatten([], _, []).
unflatten(L, N, UL) :-
    unflt(L, 1, N, UL), !.

unflt([H|T], N, N, [[H]|UL]):-
    unflatten(T, N, UL). 
unflt([H|T], N1 , N, [[H|TMP]|UL]):-
    N2 is N1 + 1,
    unflt(T, N2 , N, [TMP| UL]).

%% Delete zeros from list of results
deleteAllZeros([], []).
deleteAllZeros([H|T], [L|Result]) :-
	deleteZeros(H, L),
	deleteAllZeros(T, Result).

deleteZeros([],[]).
deleteZeros([0|T], L) :- !,
	deleteZeros(T, L).
deleteZeros([H|T], [H|T2]) :-
	deleteZeros(T, T2).


