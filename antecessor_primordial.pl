% antecessor_primordial.pl

%:- module(anti_pri,[initialize/2, execute/2, display_world/0, restart/1]).

% Antecessor Primordial World Simulator
%
% A Prolog implementation of the Antecessor Primordial World


:- dynamic
  world_initial_state/1,
  default_world_extent/1,
  world_extent/1,
  world_cold/1,
  
  enemy_tribe/2,
  enemy/2,
  wolf/2,
  weapon/2,
  pit/2,
  fire/2,
  food/2,
  
  signal_enemy_tribe_position/2,
  signal_enemy_position/2,
  signal_wolf_position/2,
  signal_weapon_position/2,
  signal_pit_position/2,
  signal_fire_position/2,
  signal_food_position/2,
  
  agent_location/2,
  agent_orientation/1,
  agent_health/1,
  agent_weapon/1,
  agent_type/1,
  agent_time_to_starve/1,
  agent_time_to_freeze/1,
  agent_score/1.
  

default_world_extent(10). 		% Default size of the world is 10x10
default_time_to_starve(6).		% Default number of rounds until the agent starves(if no food is found)
default_time_to_freeze(4).		% Default number of rounds until the agent freezes(if no fire is found)

enemy_tribe_probability(0.10).	% Probability that a non-(1,1) location has a enemy_tribe
enemy_probability(0.10). 	 	% Probability that a non-(1,1) location has an enemy
wolf_probability(0.10).   		% Probability that a non-(1,1) location has a wolf
weapon_probability(0.10). 		% Probability that a non-(1,1) location has a weapon
pit_probability(0.10).    		% Probability that a non-(1,1) location has a pit
fire_probability(0.10).   		% Probability that a non-(1,1) location has fire
food_probability(0.10).   		% Probability that a non-(1,1) location has food


% initialize(Percept,Size): initializes the Antecessor Primordial World
% and our fearless agent and returns the Percept from square 1,1.

initialize([Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food],Size) :-
  initialize_world(Size),
  
  random_member(Type,[tribe,individual]),
  initialize_agent(Type),
  
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  
  display_action(initialize).


% restart(Percept): Restarts the current world from scratch and returns
%   the initial Percept.

restart([Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food]) :-
  retractall_world,
  world_initial_state(Internal_Map),
  assert_list(Internal_Map),
  agent_type(Type),
  initialize_agent(Type),
  
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  
  display_action(restart).


% initialize_world(Size): Initializes the Antecessor Primordial world.
%	Generates a random world according to the following guidelines:
%
%   Size: The default size of the Antecessor Primordial world is 10x10,
%		but can be changed by changing the value of default_world_extent(Size).
%		If Size is set it will be used, if not the default will be used.
%			
%
%   Object Location: Each square has object with probability P set by
%				object_probability(P), except location (1,1), which
%				will never have object. At least one square will have
%				object; no more than one object type per square.
%
% world_extent(E): defines world to be E by E
% object(X,Y): there is object in square X,Y

initialize_world() :-
  default_world_extent(Size),
  initialize_world_generic(Size).

initialize_world(Size) :-
  initialize_world_generic(Size).

initialize_world_generic(Size) :-
  retractall_world,
  retractall(world_initial_state(_)),
  assert(world_initial_state([])),
  addto_world_init_state(world_extent(Size)),
  
  random_member(Cold,[yes,no]),
  addto_world_init_state(world_cold(Cold)),
  
  all_squares(Size,AllSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),	% remove agent position from valid position list

  enemy_tribe_probability(PEnemyTribe),             % place enemy_tribe(s)
  place_objects(enemy_tribe,PEnemyTribe,AllSqrs1,EnemyTribeRestSqrs),
  at_least_one_object(enemy_tribe,AllSqrs1,EnemyTribeRestSqrs,EnemySqrs),

  enemy_probability(PEnemy),             % place enemy(s)
  place_objects(enemy,PEnemy,EnemySqrs,EnemyRestSqrs),
  at_least_one_object(enemy,EnemySqrs,EnemyRestSqrs,WolfSqrs),

  wolf_probability(PWolf),             % place wolf(s)
  place_objects(wolf,PWolf,WolfSqrs,WolfRestSqrs),
  at_least_one_object(wolf,WolfSqrs,WolfRestSqrs,WeaponSqrs),
  
  weapon_probability(PWeapon),             % place weapon(s)
  place_objects(weapon,PWeapon,WeaponSqrs,WeaponRestSqrs),
  at_least_one_object(weapon,WeaponSqrs,WeaponRestSqrs,PitSqrs),
  
  pit_probability(PPit),              % place pit(s)
  place_objects(pit,PPit,PitSqrs,PitRestSqrs),
  at_least_one_object(pit,PitSqrs,PitRestSqrs,FireSqrs),
  
  fire_probability(PFire),             % place fire(s)
  place_objects(fire,PFire,FireSqrs,FireRestSqrs),
  at_least_one_object(fire,FireSqrs,FireRestSqrs,FoodSqrs),

  food_probability(PFood),             % place food(s)
  place_objects(food,PFood,FoodSqrs,FoodRestSqrs),
  at_least_one_object(food,FoodSqrs,FoodRestSqrs,_),
  
  world_initial_state(Internal_Map),
  assert_list(Internal_Map).


% initialize_agent: agent is initially alive, of type Type, destitute,
%	in grid 1,1 and facing to the right (0 degrees).

initialize_agent(Type) :-
  retractall(agent_location(_,_)),
  retractall(agent_orientation(_)),
  retractall(agent_health(_)),
  retractall(agent_weapon(_)),
  retractall(agent_type(_)),
  retractall(agent_time_to_starve(_)),
  retractall(agent_time_to_freeze(_)),
  retractall(agent_score(_)),
  assert(agent_location(1,1)),
  assert(agent_orientation(0)),
  assert(agent_health(alive)),
  assert(agent_weapon(0)),
  assert(agent_type(Type)),
  default_time_to_starve(Starve),
  assert(agent_time_to_starve(Starve)),
  default_time_to_freeze(Freeze),
  assert(agent_time_to_freeze(Freeze)),
  assert(agent_score(0)).


% retractall_world: Retract all Antecessor Primordial world information, except 
%	about the agent.

retractall_world :-
  retractall(world_extent(_)),
  retractall(world_cold(_)),
  retractall(enemy_tribe(_,_)),
  retractall(enemy(_,_)),
  retractall(wolf(_,_)),
  retractall(weapon(_,_)),
  retractall(pit(_,_)),
  retractall(fire(_,_)),
  retractall(food(_,_)).


% retractall_signal: Retract all Antecessor Primordial signal information.

retractall_signal :-
  retractall(signal_enemy_tribe_position(_,_)),
  retractall(signal_enemy_position(_,_)),
  retractall(signal_wolf_position(_,_)),
  retractall(signal_weapon_position(_,_)),
  retractall(signal_pit_position(_,_)),
  retractall(signal_fire_position(_,_)),
  retractall(signal_food_position(_,_)).
  
  
% addto_world_init_state(Fact): Adds Fact to the list Internal_Map stored in
%   world_initial_state(Internal_Map).

addto_world_init_state(Fact) :-
  retract(world_initial_state(Internal_Map)),
  assert(world_initial_state([Fact|Internal_Map])).


% assert_list(List): Assert all facts on the list List.

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


% at_least_one_object(Object,AllSqrs,ObjectRestSqrs,NextObjectTypeSqrs):
% Ensures that at least one Object is somewhere in the wumpus world

at_least_one_object(_,AllSqrs,ObjectRestSqrs,ObjectRestSqrs) :-
  \+ AllSqrs=ObjectRestSqrs,
  !.

at_least_one_object(Object,AllSqrs,AllSqrs,ObjectRestSqrs) :-
  random_member([OX,OY],AllSqrs),  % initialize Object
  Fact =.. [Object|[OX,OY]],
  delete(AllSqrs,[OX,OY],ObjectRestSqrs),
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
%   Percept = [Stench,Signal_pit,Glitter,Bump,Scream]
%             These variables are either 'yes' or 'no'.  

execute(_,[no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

execute(goforward,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  update_agent_health,    % check if agent survives movement
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),         % update rest of percept
  display_action(goforward).

execute(turnleft,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(turnleft).

execute(turnright,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(turnright).

execute(grab,[Stench,Signal_pit,no,no,no]) :-
  decrement_score,
  get_the_gold,
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(grab).

execute(shoot,[Stench,Signal_pit,Glitter,no,Scream]) :-
  decrement_score,
  shoot_arrow(Scream),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
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

execute(climb,[Stench,Signal_pit,Glitter,no,no]) :-
  decrement_score,
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(climb),
  format("You cannot leave the world from here.~n",[]).


% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).

  
% sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food):
sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food) :-
  signal_enemy_tribe(Signal_enemy_tribe),
  signal_enemy(Signal_enemy),
  signal_wolf(Signal_wolf),
  signal_weapon(Signal_weapon),
  signal_pit(Signal_pit),
  signal_fire(Signal_fire),
  signal_food(Signal_food).


% signal_enemy_tribe(Signal_enemy_tribe): Signal_enemy_tribe = yes if a enemy_tribe is in a square directly up, down,
%   left, or right of the current agent location.

signal_enemy_tribe(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( enemy_tribe(X1,Y) ;
    enemy_tribe(X0,Y) ;
    enemy_tribe(X,Y1) ;
    enemy_tribe(X,Y0) ;
    enemy_tribe(X,Y) ),
  !.

signal_enemy_tribe(no).


% signal_enemy(Signal_enemy): Signal_enemy = yes if a enemy is in a square directly up, down,
%   left, or right of the current agent location.

signal_enemy(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( enemy(X1,Y) ;
    enemy(X0,Y) ;
    enemy(X,Y1) ;
    enemy(X,Y0) ;
    enemy(X,Y) ),
  !.

signal_enemy(no).


% signal_wolf(Signal_wolf): Signal_wolf = yes if a wolf is in a square directly up, down,
%   left, or right of the current agent location.

signal_wolf(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( wolf(X1,Y) ;
    wolf(X0,Y) ;
    wolf(X,Y1) ;
    wolf(X,Y0) ;
    wolf(X,Y) ),
  !.

signal_wolf(no).


% signal_weapon(Signal_weapon): Signal_weapon = yes if a weapon is in a square directly up, down,
%   left, or right of the current agent location.

signal_weapon(yes) :-
  agent_location(X,Y),
  weapon(X,Y),
  !.

signal_weapon(no).


% signal_pit(Signal_pit): Signal_pit = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

signal_pit(yes) :-
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

signal_pit(no).


% signal_fire(Signal_fire): Signal_fire = yes if a fire is in a square directly up, down,
%   left, or right of the current agent location.

signal_fire(yes) :-
  agent_location(X,Y),
  fire(X,Y),
  !.

signal_fire(no).


% signal_food(Signal_food): Signal_food = yes if a food is in a square directly up, down,
%   left, or right of the current agent location.

signal_food(yes) :-
  agent_location(X,Y),
  food(X,Y),
  !.

signal_food(no).


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


% update_agent_health: kills agent acording to the rules of the game

update_agent_health :-
  agent_health(alive),
  agent_location(X,Y),
  (
	(enemy_tribe(X,Y),(agent_type(individual);agent_weapon(0)));
	(enemy(X,Y),agent_type(individual),agent_weapon(0));
	(wolf(X,Y),agent_type(individual),agent_weapon(0));
	pit(X,Y);
	agent_time_to_starve(0);
	agent_time_to_freeze(0)
  ),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("You just died!~n",[]).

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
  agent_orientation(AA),
  agent_health(AH),
  agent_weapon(N),
  format('agent_orientation(~d)~n',[AA]),
  format('agent_health(~w)~n',[AH]),
  format('agent_weapon(~d)~n',[N]).

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
  display_location_fact(enemy_tribe,X,Y,'T'),
  display_location_fact(enemy,X,Y,'E'),
  display_location_fact(wolf,X,Y,'W'),
  display_location_fact(weapon,X,Y,'X'),
  display_location_fact(pit,X,Y,'P'),
  display_location_fact(fire,X,Y,'F'),
  display_location_fact(food,X,Y,'Q'),
  write(' ').

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w',[Atom]).

display_location_fact(_,_,_,_) :-
  format(' ',[]).

display_dashes(E) :-
  RowLen is (E * 10) + 1,
  name('-',[Dash]),
  format('~*c~n',[RowLen,Dash]).

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
