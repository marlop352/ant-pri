% wumpus_world.pl

:- module(wumpus,[initialize/2, execute/2, display_world/0]).

% AM
:- use_module(library(lists)).

% Wumpus World Simulator v2.3
%
% Written by Larry Holder (holder@cse.uta.edu)
%
% A Prolog implementation of the Wumpus world described in Russell and
% Norvig's "Artificial Intelligence: A Modern Approach", Section 6.2.
% A few enhancements have been added:
%   - random wumpus world generator
%
% See comments on the following interface procedures:
%
%   evaluate_agent(Trials,Score,Time)
%   initialize(World,Percept)

%WNb
% modified for SWI-Prolog
% without the following enhancements:
%     natural language hint, movement of wumpus, image processing, several tries per world
% Walter Nauber
% 02.02.01
% changes: max_agent_actions = 8*n*n

:- load_files([utils]).  % Basic utilities

:- dynamic  wumpus_world_default_extent/1, max_agent_actions/1.   
:- dynamic  wumpus_world_extent/1,  wumpus_location/2, wumpus_health/1,  gold/2,  pit/2.
:- dynamic  agent_location/2,  agent_orientation/1,  agent_in_cave/1,  agent_health/1.
:- dynamic  agent_gold/1,  agent_arrows/1,  agent_score/1.

wumpus_world_default_extent(4).  % Default size of the cave is 4x4
%WNe


gold_probability(0.10).  % Probability that a location has gold
pit_probability(0.15).   % Probability that a non-(1,1) location has a pit


% initialize(World,Percept): initializes the Wumpus world and our fearless
%   agent according to the given World and returns the Percept from square
%   1,1.  World can be either 'fig62' for Figure 6.2 of Russell and Norvig,
%   or 'random' to generate a random world.

initialize(World,[Stench,Breeze,Glitter,no,no]) :-
  initialize_world(World),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter).
%  display_action(initialize).



% initialize_world(World): Initializes the Wumpus world.  World is either
%   fig62, which generates the wumpus world in Figure 6.2 of [Russell &
%   Norvig], or World=random, which generates a random world according to
%   the following guidelines:
%
%   Size: The size of the wumpus world is fixed at 4x4, but can be set
%         arbitrarily using different values for wumpus_world_extent(E).
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
  wumpus_world_default_extent(E),
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
%WNb
  addto_ww_init_state(wumpus_world_extent(E)),
  all_squares(E,AllSqrs),
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs,GRestSqrs),
  at_least_one_gold(E,AllSqrs,GRestSqrs,PSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),   % deletes element [1,1] from list AllSqrs
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  delete(PSqrs,[1,1],PSqrs1),       % deletes element [1,1] from list PSqrs
  delete(PSqrs1,[WX,WY],PSqrs2),    % deletes element [WX,WY] from list PSqrs1
  pit_probability(PP),              % place pits
  place_objects(pit,PP,PSqrs2,_),
  
%WNe
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  ww_initial_state(L),
  assert_list(L).


% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

%WNb
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
%WNe
  addto_ww_init_state(gold(X,Y)).
