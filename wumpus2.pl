% Wumpus World Simulator
%
% Adapted from code written by Larry Holder (holder@cse.uta.edu)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
% A few enhancements have been added:
%   - random wumpus world generator
%	- random wumpus world generator can use a arbitrary size
%
% See comments on the following interface procedures:
%
%   initialize(World,Percept[,Size])
%   restart(Percept)
%


:- dynamic
  ww_initial_state/1,
  wumpus_world_extent/1,
  wumpus_location/2,
  wumpus_health/1,
  gold/2,
  pit/2,
  agent_location/2,
  agent_orientation/1,
  agent_in_cave/1,
  agent_health/1,
  agent_gold/1,
  agent_arrows/1,
  agent_score/1.

wumpus_world_default_extent(4).  % Default size of the cave is 4x4
gold_probability(0.10).  % Probability that a location has gold
pit_probability(0.20).   % Probability that a non-(1,1) location has a pit


% initialize(World,Percept[,Size]): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  World can be either 'fig62' for Figure 6.2 of Russell and Norvig,
%   or 'random' to generate a random world.

initialize(World,[Stench,Breeze,Glitter,no,no]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(initialize).

initialize(World,[Stench,Breeze,Glitter,no,no],Size) :-
  initialize_world(World,Size),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(initialize).


% restart(Percept): Restarts the current world from scratch and returns
%   the initial Percept.

restart([Stench,Breeze,Glitter,no,no]) :-
  ww_retractall,
  ww_initial_state(L),
  assert_list(L),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(restart).


% initialize_world(World[,Size]): Initializes the Wumpus world.  World is either
%   fig62, which generates the wumpus world in Figure 6.2 of [Russell &
%   Norvig], or World=random, which generates a random world according to
%   the following guidelines:
%
%   Size: The default size of the wumpus world is 4x4, but can be changed
% 			by changing the value of wumpus_world_default_extent(Size).
%			If Size is set it will be used, if not the default will be used.
%			Size is only valid in the random world.
%			
%
%   Wumpus Location: The initial wumpus location is chosen at random
%                    anywhere in the cave except location (1,1).
%
%   Pit Location: Each square has a pit with probability P set by
%                 pit_probability(P), except location (1,1), which
%                 will never have a pit.
%
%   Gold Location: Each square has gold with probability P set by
%                  gold_probability(P).  At least one square will have
%                  gold; no more than one gold piece per square.
%
% wumpus_world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
%                        initially nil.
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world(fig62) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(4)),
  addto_ww_init_state(wumpus_location(1,3)),
  addto_ww_init_state(wumpus_health(alive)),
  addto_ww_init_state(gold(2,3)),
  addto_ww_init_state(pit(3,1)),
  addto_ww_init_state(pit(3,3)),
  addto_ww_init_state(pit(4,4)),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random) :-
  wumpus_world_default_extent(WWS),
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(WWS)),
  all_squares(WWS,AllSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),	% remove agent position from valid position list
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  delete(AllSqrs1,[WX,WY],AllSqrs2),	% remove wumpus position from valid position list
  
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs2),
  at_least_one_gold(WWS),
  
  pit_probability(PP),              % place pits
  place_objects(pit,PP,AllSqrs2),
  ww_initial_state(L),
  assert_list(L).

initialize_world(random,Size) :-
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(Size)),
  all_squares(Size,AllSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),	% remove agent position from valid position list
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  delete(AllSqrs1,[WX,WY],AllSqrs2),	% remove wumpus position from valid position list
  
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs2),
  at_least_one_gold(Size),
  
  pit_probability(PP),              % place pits
  place_objects(pit,PP,AllSqrs2),
  ww_initial_state(L),
  assert_list(L).

%generic1.pl

% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

place_objects(_,_,[]).

place_objects(Object,P,[Square|Squares]) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_ww_init_state(Fact),
  place_objects(Object,P,Squares).

place_objects(Object,P,[_|Squares]) :-
  place_objects(Object,P,Squares).


% at_least_one_gold(Extent): Ensures that at least on gold piece is
%   somewhere in the wumpus world.

at_least_one_gold(E) :-
  random_between(1,E,X),
  (X == 1 *-> random_between(2,E,Y) ; random_between(1,E,Y)),
  addto_ww_init_state(gold(X,Y)).

  
%execute.pl
%score.pl
%signals.pl
%actions.pl
  