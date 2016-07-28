% antecessor_primordial.pl

:- module(anti_pri,[initialize/2, execute/2, display_world/0, restart/1]).

% Antecessor Primordial World Simulator
%
% A Prolog implementation of the Antecessor Primordial World


:- dynamic
  world_initial_state/1,
  world_default_extent/1,
  world_extent/1,
  
  tribe_amount/1,
  tribe/5,
  
  enemy_amount/1,
  enemy/5,
  
  wolf_amount/1,
  wolf/2,
  
  weapon_amount/1,
  weapon/2,
  
  pit_amount/1,
  pit/2,
  
  world_cold/1,
  
  fire_amount/1,
  fire/2,
    
  food_amount/1,
  food/2,
  
  agent_location/2,
  agent_orientation/1,
  agent_tribe/1,
  agent_health/1,
  
  agent_hunger/1,
  agent_time_to_starve/1,
  
  agent_time_to_freeze/1,
  
  agent_weapon/1,
  agent_score/1.
  

world_default_extent(10). % Default size of the world is 10x10
tribe_probability(0.10).  % Probability that a non-(1,1) location has a tribe
enemy_probability(0.10).  % Probability that a non-(1,1) location has an enemy
wolf_probability(0.10).   % Probability that a non-(1,1) location has a wolf
weapon_probability(0.10). % Probability that a non-(1,1) location has a weapon
pit_probability(0.10).    % Probability that a non-(1,1) location has a pit
fire_probability(0.10).   % Probability that a non-(1,1) location has fire
food_probability(0.10).   % Probability that a non-(1,1) location has food


% initialize(Percept[,Size]): initializes the Antecessor Primordial World
% and our fearless agent and returns the Percept from square 1,1.

initialize([Bark,Scream_enemy,Scream_tribe,Breeze,Freezing]) :-
  initialize_world(),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(initialize).

initialize([Stench,Breeze,Glitter,no,no],Size) :-
  initialize_world(Size),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(initialize).


% restart(Percept): Restarts the current world from scratch and returns
%   the initial Percept.

restart([Stench,Breeze,Glitter,no,no]) :-
  world_retractall,
  world_initial_state(L),
  assert_list(L),
  initialize_agent,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(restart).


% initialize_world(Size): Initializes the Wumpus world.  Generates a
%   random world according to the following guidelines:
%
%   Size: The default size of the wumpus world is 4x4, but can be changed
% 			by changing the value of world_default_extent(Size).
%			If Size is set it will be used, if not the default will be used.
%			Size is only valid in the random world.
%			
%
%   Wumpus Location: The initial wumpus location is chosen at random
%                    anywhere in the world except location (1,1).
%
%   Pit Location: Each square has a pit with probability P set by
%                 pit_probability(P), except location (1,1), which
%                 will never have a pit.
%
%   Gold Location: Each square has gold with probability P set by
%                  gold_probability(P).  At least one square will have
%                  gold; no more than one gold piece per square.
%
% world_extent(E): defines world to be E by E
% wumpus_location(X,Y): the Wumpus is in square X,Y
% wumpus_health(H): H is 'dead' or 'alive'
%                        initially nil.
% gold(X,Y): there is gold in square X,Y
% pit(X,Y): there is a pit in square X,Y

initialize_world() :-
  world_default_extent(Size),
  initialize_world_generic(Size).

initialize_world(Size) :-
  initialize_world_generic(Size).

initialize_world_generic(Size) :-
  world_retractall,
  retractall(world_initial_state(_)),
  assert(world_initial_state([])),
  addto_world_init_state(world_extent(Size)),
  all_squares(Size,AllSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),	% remove agent position from valid position list
  
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  addto_world_init_state(wumpus_location(WX,WY)),
  addto_world_init_state(wumpus_health(alive)),
  delete(AllSqrs1,[WX,WY],AllSqrs2),	% remove wumpus position from valid position list
  
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs2,GRestSqrs),
  at_least_one_object(gold,AllSqrs2,GRestSqrs,PSqrs),
  
  pit_probability(PP),              % place pits
  place_objects(pit,PP,PSqrs,PRestSqrs),
  at_least_one_object(pit,PSqrs,PRestSqrs,_),
  
  world_initial_state(L),
  assert_list(L).


% initialize_agent: agent is initially alive, destitute (except for one
%   arrow), in grid 1,1 and facing to the right (0 degrees).

initialize_agent :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent_in_cave(_)),
  retractall(agent_health(_)),
  retractall(agent_gold(_)),
  retractall(agent_arrows(_)),
  retractall(agent_score(_)),
  assert(agent_location(1,1)),
  assert(agent_orientation(0)),
  assert(agent_in_cave(yes)),
  assert(agent_health(alive)),
  assert(agent_gold(0)),
  assert(agent_arrows(1)),
  assert(agent_score(0)).


% world_retractall: Retract all wumpus world information, except about the
%   agent.

world_retractall :-
  retractall(world_extent(_)),
  retractall(wumpus_location(_,_)),
  retractall(wumpus_health(_)),
  retractall(gold(_,_)),
  retractall(pit(_,_)).


% addto_world_init_state(Fact): Adds Fact to the list L stored in
%   world_initial_state(L).

addto_world_init_state(Fact) :-
  retract(world_initial_state(L)),
  assert(world_initial_state([Fact|L])).


% assert_list(L): Assert all facts on list L.

assert_list([]).

assert_list([Fact|Facts]) :-
  assert(Fact),
  assert_list(Facts).


% all_squares(Extent,AllSqrs): AllSqrs is the list of all possible
%   squares [X,Y] in a wumpus world of size Extent by Extent.

all_squares(Extent,AllSqrs) :-
  all_squares_1(Extent,1,1,AllSqrs).

all_squares_1(Extent,Extent,Extent,[[Extent,Extent]]).

all_squares_1(Extent,Row,Extent,[[Row,Extent]|RestSqrs]) :-
  Row < Extent,
  Row1 is Row + 1,
  all_squares_1(Extent,Row1,1,RestSqrs).

all_squares_1(Extent,Row,Col,[[Row,Col]|RestSqrs]) :-
  Col < Extent,
  Col1 is Col + 1,
  all_squares_1(Extent,Row,Col1,RestSqrs).


% place_objects(Object,P,Squares): For each square in Squares, place
%   Object at square with probability P.

place_objects(_,_,[],[]).

place_objects(Object,P,[Square|Squares],RestSquares) :-
  maybe(P),   % succeeds with probability P
  !,
  Fact =.. [Object|Square],
  addto_world_init_state(Fact),
  place_objects(Object,P,Squares,RestSquares).

place_objects(Object,P,[Square|Squares],[Square|RestSquares]) :-
  place_objects(Object,P,Squares, RestSquares).


% at_least_one_object(Object,AllSqrs,ORestSqrs,RSqrs):
% Ensures that at least one Object is somewhere in the wumpus world

at_least_one_object(_,AllSqrs,ORestSqrs,ORestSqrs) :-
  \+ AllSqrs=ORestSqrs,
  !.

at_least_one_object(Object,AllSqrs,AllSqrs,ORestSqrs) :-
  random_member([OX,OY],AllSqrs),  % initialize gold
  Fact =.. [Object|[OX,OY]],
  delete(AllSqrs,[OX,OY],ORestSqrs),
  addto_world_init_state(Fact).


%------------------------------------------------------------------------
% execute(Action,Percept): executes Action and returns Percept
%
%   Action is one of:
%     goforward: move one square along current orientation if possible
%     turnleft:  turn left 90 degrees
%     turnright: turn right 90 degrees
%     grab:      pickup gold if in square
%     shoot:     shoot an arrow along orientation, killing wumpus if
%                in that direction
%     climb:     if in square 1,1, leaves the cave and adds 1000 points
%                for each piece of gold
%
%   Percept = [Stench,Breeze,Glitter,Bump,Scream]
%             These variables are either 'yes' or 'no'.  

execute(_,[no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

execute(_,[no,no,no,no,no]) :-
  agent_in_cave(no), !,         % agent must be in the cave
  format("You have left the world.~n",[]).

execute(goforward,[Stench,Breeze,Glitter,Bump,no]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  update_agent_health,    % check for wumpus or pit
  stench(Stench),         % update rest of percept
  breeze(Breeze),
  glitter(Glitter),
  display_action(goforward).

execute(turnleft,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnleft).

execute(turnright,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(turnright).

execute(grab,[Stench,Breeze,no,no,no]) :-
  decrement_score,
  get_the_gold,
  stench(Stench),
  breeze(Breeze),
  display_action(grab).

execute(shoot,[Stench,Breeze,Glitter,no,Scream]) :-
  decrement_score,
  shoot_arrow(Scream),
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(shoot).

execute(climb,[no,no,no,no,no]) :-  % climb works
  agent_location(1,1), !,
  decrement_score,
  agent_gold(G),
  retract(agent_score(S)),
  S1 is (S + (1000 * G)),
  assert(agent_score(S1)),
  retract(agent_in_cave(yes)),
  assert(agent_in_cave(no)),
  display_action(climb),
  format("I am outta here.~n",[]).

execute(climb,[Stench,Breeze,Glitter,no,no]) :-
  decrement_score,
  stench(Stench),
  breeze(Breeze),
  glitter(Glitter),
  display_action(climb),
  format("You cannot leave the world from here.~n",[]).


% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).


% stench(Stench): Stench = yes if wumpus (dead or alive) is in a square
%   directly up, down, left, or right of the current agent location.

stench(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( wumpus_location(X1,Y) ;
    wumpus_location(X0,Y) ;
    wumpus_location(X,Y1) ;
    wumpus_location(X,Y0) ;
    wumpus_location(X,Y) ),
  !.

stench(no).


% breeze(Breeze): Breeze = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

breeze(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( pit(X1,Y) ;
    pit(X0,Y) ;
    pit(X,Y1) ;
    pit(X,Y0) ;
    pit(X,Y)  ),
  !.

breeze(no).


% glitter(Glitter): Glitter = yes if there is gold in the current agent
%   location.

glitter(yes) :-
  agent_location(X,Y),
  gold(X,Y),
  !.

glitter(no).


% kill_wumpus: pretty obvious

kill_wumpus :-
  retract(wumpus_health(alive)),
  assert(wumpus_health(dead)).


% goforward(Bump): Attempts to move agent forward one unit along
%   its current orientation.

goforward(no) :-
  agent_orientation(Angle),
  agent_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  world_extent(E),         % check if agent off world
  X1 > 0,
  X1 =< E,
  Y1 > 0,
  Y1 =< E,
  !,
  retract(agent_location(X,Y)),   % update location
  assert(agent_location(X1,Y1)).

goforward(yes).     % Ran into wall, Bump = yes


% new_location(X,Y,Orientation,X1,Y1): returns new coordinates X1,Y1
%   after moving from X,Y along Orientation: 0, 90, 180, 270 degrees.

new_location(X,Y,0,X1,Y) :-
  X1 is X + 1.

new_location(X,Y,90,X,Y1) :-
  Y1 is Y + 1.

new_location(X,Y,180,X1,Y) :-
  X1 is X - 1.

new_location(X,Y,270,X,Y1) :-
  Y1 is Y - 1.


% update_agent_health: kills agent if in a room with a live wumpus or a
%   pit.

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  wumpus_health(alive),
  wumpus_location(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("You are Wumpus food!~n",[]).

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  pit(X,Y),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("Aaaaaaaaaaaaaaaaaaa!~n",[]).

update_agent_health.


% get_the_gold: adds gold to agents loot if any gold in the square

get_the_gold :-
  agent_location(X,Y),
  gold(X,Y), !,                   % there's gold in this square!
  agent_gold(NGold),              %   add to agents loot
  NGold1 is NGold + 1,
  retract(agent_gold(NGold)),
  assert(agent_gold(NGold1)),
  format("You now have ~d piece(s) of gold!~n",NGold1),
  retract(gold(X,Y)).             %   delete gold from square

get_the_gold.


% shoot_arrow(Scream): If agent has an arrow, then shoot it in the
%   direction the agent is facing and listen for Scream.

shoot_arrow(Scream) :-
  agent_arrows(Arrows),
  Arrows > 0, !,                  % agent has an arrow and will use it!
  Arrows1 is Arrows - 1,          %   update number of arrows
  retract(agent_arrows(Arrows)),
  assert(agent_arrows(Arrows1)),
  format("You now have ~d arrow(s).~n",Arrows1),
  agent_location(X,Y),
  agent_orientation(Angle),
  propagate_arrow(X,Y,Angle,Scream).

shoot_arrow(no).


% propagate_arrow(X,Y,Angle,Scream): If wumpus is at X,Y then hear its
%   woeful scream as you vanquish the creature.  If not, then move arrow
%   one square along Angle and try again.  If arrow hits a wall, then
%   you missed.

propagate_arrow(X,Y,_,yes) :-
  wumpus_location(X,Y), !,
  kill_wumpus,
  retract(agent_score(S)),
  S1 is (S + 500),
  assert(agent_score(S1)).

propagate_arrow(X,Y,0,Scream) :-
  X1 is X + 1,
  world_extent(E),
  X1 =< E,
  !,
  propagate_arrow(X1,Y,0,Scream).

propagate_arrow(X,Y,90,Scream) :-
  Y1 is Y + 1,
  world_extent(E),
  Y1 =< E,
  !,
  propagate_arrow(X,Y1,90,Scream).

propagate_arrow(X,Y,180,Scream) :-
  X1 is X - 1,
  X1 > 0,
  !,
  propagate_arrow(X1,Y,180,Scream).

propagate_arrow(X,Y,270,Scream) :-
  Y1 is Y - 1,
  Y1 > 0,
  !,
  propagate_arrow(X,Y1,270,Scream).

propagate_arrow(_,_,_,no).


% display_world: Displays everything known about the wumpus world,

display_world :-
  nl,
  world_extent(E),
  display_rows(E,E),
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