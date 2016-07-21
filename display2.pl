% display_world: Displays everything known about the wumpus world,

display_world :-
  nl,
  wumpus_world_extent(E),
  display_rows(E,E).
/*  
  wumpus_health(WH),
  agent_orientation(AA),
  agent_health(AH),
  agent_arrows(N),
  agent_gold(G),
  format('wumpus_health(~w)~n',[WH]),
  format('agent_orientation(~d)~n',[AA]),
  format('agent_health(~w)~n',[AH]),
  format('agent_arrows(~d)~n',[N]),
  format('agent_gold(~d)~n',[G]).
*/

display_rows(0,E) :-
  !,
  display_dashes(E).

display_rows(Row,E) :-
  display_dashes(E),
  display_row(Row,E),
  Row1 is Row - 1,
  display_rows(Row1,E).

display_row(Row,E) :-
  display_square(1,Row,E).

display_square(X,_,E) :-
  X > E,
  !,
  format('|~n',[]).

display_square(X,Y,E) :-
  format('|',[]),
  display_info(X,Y),
  X1 is X + 1,
  display_square(X1,Y,E).

display_info(X,Y) :-
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_location_fact(gold,X,Y,'G'),
  display_location_fact(pit,X,Y,'P'),
  write(' '),
  wumpus_health(WH),
  display_wumpus(WH,WC),
  display_location_fact(wumpus_location,X,Y,WC).

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w',[Atom]).

display_location_fact(_,_,_,_) :-
  format(' ',[]).

display_dashes(E) :-
  RowLen is (E * 6) + 1,
  name('-',[Dash]),
  format('~*c~n',[RowLen,Dash]).

%display_wumpus(Wumpus_Health,Wumpus_Char)
display_wumpus(alive,'W').
display_wumpus(dead, 'd').

%display_agent(Agent_Orientation,Agent_Char)
display_agent(  0,'>').
display_agent( 90,'^').
display_agent(180,'<').
display_agent(270,'V').

% display_action(Action): Updates display after Action taken and
%   new percept generated.

display_action(Action) :-
  format("~nExecuting ~w~n",[Action]),
  display_world.
%  (((\+ hidden_cave(yes)) -> display_world);true).
