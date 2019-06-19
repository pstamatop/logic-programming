:- set_flag(print_depth, 1000).

:- lib(ic).
:- lib(branch_and_bound).

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.


maxclq(Nodes, Density, Clique, Size) :-
   create_graph(Nodes, Density, Graph),
   length(Solution, Nodes),
   Solution #:: 0..1,
   constrain(Solution, Graph, 2, Nodes),
   Cost #= Nodes - sum(Solution),
   bb_min(search(Solution, 0, first_fail, indomain, complete, []), Cost, bb_options{strategy:restart}),
   Size is Nodes - Cost,
   buildClique(Solution, 1, Clique), !.


constrain(_, _, I, Nodes) :- 
   I > Nodes.
constrain(Solution, Graph, I, Nodes) :-
   I =< Nodes,
   pairs(Solution, Graph, I, 1),
   Next is I + 1,
   constrain(Solution, Graph, Next, Nodes).


pairs(_, _, I, J) :- 
   J = I.
pairs(Solution, Graph, I, J) :-
   J < I,
   member(J-I, Graph), !,
   Next is J + 1,
   pairs(Solution, Graph, I, Next).
pairs(Solution, Graph, I, J) :-
   J < I,
   Next is J + 1,
   getNth(Solution, I, X),
   getNth(Solution, J, Y),
   X + Y #=< 1,
   pairs(Solution, Graph, I, Next).


getNth([X|_], 1, X) :- !.
getNth([_|Y], I, Ret) :-
   I > 1,
   I1 is I - 1,
   getNth(Y, I1, Ret).


buildClique([], _, []).
buildClique([X|Y], Index, [Index|Res]) :-
   X == 1,
   I is Index + 1,
   buildClique(Y, I, Res), !.
buildClique([X|Y], Index, Res) :-
   X == 0,
   I is Index + 1,
   buildClique(Y, I, Res), !.
