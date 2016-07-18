% Utilities

:- op(500,fx,not).

not(P) :- P, !, fail ; true.


member(X,[X|_]).

member(X,[_|R]) :-
  member(X,R).


conc([],L,L).

conc([H|T],L1,[H|L2]) :-
  conc(T,L1,L2).


% del(X,L1,L2): True if L2 is L1 minus one occurrence of X.

del(X,[X|L],L).

del(X,[Y|L1],[Y|L2]) :-
  del(X,L1,L2).
