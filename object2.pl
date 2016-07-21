% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

place_objects(_,_,[],[]).

place_objects(Object,P,[Square|Squares],RestSquares) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares,RestSquares).

place_objects(Object,P,[Square|Squares],[Square|RestSquares]) :-
  place_objects(Object,P,Squares, RestSquares).


% at_least_one_gold(Extent,AllSqrs,GRestSqrs,PSqrs):
% Ensures that at least on gold piece is somewhere in the wumpus world

at_least_one_gold(_,AllSqrs,GRestSqrs,GRestSqrs) :-
  \+ AllSqrs=GRestSqrs,
  !.

at_least_one_gold(E,AllSqrs,AllSqrs,GRestSqrs) :-
  random3(1,E,X),
  random3(1,E,Y),
  delete(AllSqrs,[X,Y],GRestSqrs),
  addto_ww_init_state(gold(X,Y)).
