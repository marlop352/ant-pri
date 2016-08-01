% antecessor_primordial.pl

%:- module(anti_pri,[initialize/2, execute/2, display_world/0, restart/1]).

% Antecessor Primordial World Simulator
%
% A Prolog implementation of the Antecessor Primordial World


:- dynamic([				% world stuff
  world_initial_state/1,
  default_world_extent/1,
  world_extent/1,
  world_cold/1]).

:- dynamic([				% known objects stuff
  enemy_tribe/3,
  enemy/3,
  wolf/3,
  weapon/3,
  pit/3,
  fire/3,
  food/3]).

:- dynamic([				% known signals stuff
  signal_enemy_tribe/3,
  signal_enemy/3,
  signal_wolf/3,
  signal_weapon/3,
  signal_pit/3,
  signal_fire/3,
  signal_food/3]).

:- dynamic([				% agent stuff
  agent_location/2,
  agent_orientation/1,
  agent_health/1,
  agent_weapon/1,
  agent_type/1,
  agent_time_to_starve/1,
  agent_time_to_freeze/1,
  agent_score/1]).

:- dynamic([				% map stuff
  map_type/1]).
  
set_map_type(Type) :- retractall(map_type(_)),assert(map_type(Type)).
:- set_map_type(info).

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

restart([Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,no]) :-
  retractall_world,
  retractall_signal,
  world_initial_state(Internal_Map),
  assert_list(Internal_Map),
  agent_type(Type),
  initialize_agent(Type),
  
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  
  display_action(restart).


% resurrect_agent(Percept): Resurrect the agent returns the Percept.

resurrect_agent([Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,no]) :-
  retractall(agent_health(_)),
  retractall(agent_time_to_starve(_)),
  retractall(agent_time_to_freeze(_)),
  default_time_to_starve(Starve),
  assert(agent_time_to_starve(Starve)),
  default_time_to_freeze(Freeze),
  assert(agent_time_to_freeze(Freeze)),
  assert(agent_health(alive)),
  
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  
  display_action(resurrect).


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
  retractall_signal,
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
  place_signal(signal_enemy_tribe,AllSqrs),

  enemy_probability(PEnemy),             % place enemy(s)
  place_objects(enemy,PEnemy,EnemySqrs,EnemyRestSqrs),
  at_least_one_object(enemy,EnemySqrs,EnemyRestSqrs,WolfSqrs),
  place_signal(signal_enemy,AllSqrs),

  wolf_probability(PWolf),             % place wolf(s)
  place_objects(wolf,PWolf,WolfSqrs,WolfRestSqrs),
  at_least_one_object(wolf,WolfSqrs,WolfRestSqrs,WeaponSqrs),
  place_signal(signal_wolf,AllSqrs),
  
  weapon_probability(PWeapon),             % place weapon(s)
  place_objects(weapon,PWeapon,WeaponSqrs,WeaponRestSqrs),
  at_least_one_object(weapon,WeaponSqrs,WeaponRestSqrs,PitSqrs),
  place_signal(signal_weapon,AllSqrs),
  
  pit_probability(PPit),              % place pit(s)
  place_objects(pit,PPit,PitSqrs,PitRestSqrs),
  at_least_one_object(pit,PitSqrs,PitRestSqrs,FireSqrs),
  place_signal(signal_pit,AllSqrs),
  
  fire_probability(PFire),             % place fire(s)
  place_objects(fire,PFire,FireSqrs,FireRestSqrs),
  at_least_one_object(fire,FireSqrs,FireRestSqrs,FoodSqrs),
  place_signal(signal_fire,AllSqrs),

  food_probability(PFood),             % place food(s)
  place_objects(food,PFood,FoodSqrs,FoodRestSqrs),
  at_least_one_object(food,FoodSqrs,FoodRestSqrs,_),
  place_signal(signal_food,AllSqrs),
  
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
  retractall(enemy_tribe(_,_,_)),
  retractall(enemy(_,_,_)),
  retractall(wolf(_,_,_)),
  retractall(weapon(_,_,_)),
  retractall(pit(_,_,_)),
  retractall(fire(_,_,_)),
  retractall(food(_,_,_)).


% retractall_signal: Retract all Antecessor Primordial signal information.

retractall_signal :-
  retractall(signal_enemy_tribe(_,_,_)),
  retractall(signal_enemy(_,_,_)),
  retractall(signal_wolf(_,_,_)),
  retractall(signal_weapon(_,_,_)),
  retractall(signal_pit(_,_,_)),
  retractall(signal_fire(_,_,_)),
  retractall(signal_food(_,_,_)).
  
  
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
  segment_square(Square,X,Y),
  Fact =.. [Object,X,Y,yes],
  addto_world_init_state(Fact),
  place_objects(Object,P,Squares,RestSquares).

place_objects(Object,P,[Square|Squares],[Square|RestSquares]) :-
  place_objects(Object,P,Squares, RestSquares).


% place_signal(Signal,Squares): For each square in Squares, place
%   Object at square with probability P.

place_signal(_,[]).

place_signal(Signal,[Square|Squares]) :-
  segment_square(Square,X,Y),
  Fact =.. [Signal,X,Y,unknown],
  addto_world_init_state(Fact),
  place_signal(Signal,Squares).

segment_square(Square,X,Y) :-
  Segment =.. [position|Square],
  assert(Segment),
  position(X,Y),
  retractall(position(_,_)).


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
%
%   Percept = [Stench,Signal_pit,Glitter,Bump,Scream]
%             These variables are either 'yes' or 'no'.  

execute(_,[no,no,no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("Você está morto!~n",[]).

execute(goforward,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,Bump]) :-
  decrement_score,
  goforward(Bump),        % update location and check for bump
  decrement_status,
  update_agent_health,    % check if agent survives movement
  (agent_health(alive) *-> (agent_location(X,Y),agent_kills(X,Y)) ; true),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),         % update rest of percept
  display_action(goforward).

execute(turnleft,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 90) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(turnleft).

execute(turnright,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,no]) :-
  decrement_score,
  agent_orientation(Angle),
  NewAngle is (Angle + 270) mod 360,
  retract(agent_orientation(Angle)),
  assert(agent_orientation(NewAngle)),
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(turnright).

execute(grab,[Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food,no]) :-
  decrement_score,
  get_food,
  get_weapon,
  get_fire,
  decrement_status,
  update_agent_health,    % check if agent survives movement
  sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food),
  display_action(grab).


% decrement_score: subtracts one from agent_score for each move

decrement_score :-
  retract(agent_score(S)),
  S1 is S - 1,
  assert(agent_score(S1)).

  
% decrement_status: subtracts one from agent_time_to_starve and agent_time_to_freeze

decrement_status :-
  retract(agent_time_to_starve(Starve)),
  Starve1 is Starve - 1,
  assert(agent_time_to_starve(Starve1)),
  (
    (world_cold(Cold),Cold==yes) *-> (
      retract(agent_time_to_freeze(Freeze)),
      Freeze1 is Freeze - 1,
      assert(agent_time_to_freeze(Freeze1))
	) ; true 
  ).

  
% sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food):
sense(Signal_enemy_tribe,Signal_enemy,Signal_wolf,Signal_weapon,Signal_pit,Signal_fire,Signal_food) :-
  is_signal_enemy_tribe(Signal_enemy_tribe),
  ( is_enemy_tribe(X,Y) *-> true ; assert(enemy_tribe(X,Y,no)) ),
  is_signal_enemy(Signal_enemy),
  ( is_enemy(X,Y) *-> true ; assert(enemy(X,Y,no)) ),
  is_signal_wolf(Signal_wolf),
  ( is_wolf(X,Y) *-> true ; assert(wolf(X,Y,no)) ),
  is_signal_weapon(Signal_weapon),
  ( is_weapon(X,Y) *-> true ; assert(weapon(X,Y,no)) ),
  is_signal_pit(Signal_pit),
  ( is_pit(X,Y) *-> true ; assert(pit(X,Y,no)) ),
  is_signal_fire(Signal_fire),
  ( is_fire(X,Y) *-> true ; assert(fire(X,Y,no)) ),
  is_signal_food(Signal_food),
  ( is_food(X,Y) *-> true ; assert(food(X,Y,no)) ).


is_enemy_tribe(X,Y) :-
  enemy_tribe(X,Y,yes).

is_enemy(X,Y) :-
  enemy(X,Y,yes).

is_wolf(X,Y) :-
  wolf(X,Y,yes).

is_weapon(X,Y) :-
  weapon(X,Y,yes).

is_pit(X,Y) :-
  pit(X,Y,yes).

is_fire(X,Y) :-
  fire(X,Y,yes).

is_food(X,Y) :-
  food(X,Y,yes).


% is_signal_enemy_tribe(Signal_enemy_tribe): Signal_enemy_tribe = yes if a enemy_tribe is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_enemy_tribe(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( is_enemy_tribe(X1,Y) ;
    is_enemy_tribe(X0,Y) ;
    is_enemy_tribe(X,Y1) ;
    is_enemy_tribe(X,Y0) ),
  retractall(signal_enemy_tribe(X,Y,_)),
  assert(signal_enemy_tribe(X,Y,yes)),
  !.

is_signal_enemy_tribe(no) :-
  agent_location(X,Y),
  retractall(signal_enemy_tribe(X,Y,_)),
  assert(signal_enemy_tribe(X,Y,no)),
  !.


% is_signal_enemy(Signal_enemy): Signal_enemy = yes if a enemy is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_enemy(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( is_enemy(X1,Y) ;
    is_enemy(X0,Y) ;
    is_enemy(X,Y1) ;
    is_enemy(X,Y0) ),
  retractall(signal_enemy(X,Y,_)),
  assert(signal_enemy(X,Y,yes)),
  !.

is_signal_enemy(no) :-
  agent_location(X,Y),
  retractall(signal_enemy(X,Y,_)),
  assert(signal_enemy(X,Y,no)),
  !.


% is_signal_wolf(Signal_wolf): Signal_wolf = yes if a wolf is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_wolf(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( is_wolf(X1,Y) ;
    is_wolf(X0,Y) ;
    is_wolf(X,Y1) ;
    is_wolf(X,Y0) ),
  retractall(signal_wolf(X,Y,_)),
  assert(signal_wolf(X,Y,yes)),
  !.

is_signal_wolf(no) :-
  agent_location(X,Y),
  retractall(signal_wolf(X,Y,_)),
  assert(signal_wolf(X,Y,no)),
  !.


% is_signal_weapon(Signal_weapon): Signal_weapon = yes if a weapon is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_weapon(yes) :-
  agent_location(X,Y),
  is_weapon(X,Y),
  retractall(signal_weapon(X,Y,_)),
  assert(signal_weapon(X,Y,yes)),
  !.

is_signal_weapon(no) :-
  agent_location(X,Y),
  retractall(signal_weapon(X,Y,_)),
  assert(signal_weapon(X,Y,no)),
  !.


% is_signal_pit(Signal_pit): Signal_pit = yes if a pit is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_pit(yes) :-
  agent_location(X,Y),
  X1 is X + 1,
  X0 is X - 1,
  Y1 is Y + 1,
  Y0 is Y - 1,
  ( is_pit(X1,Y) ;
    is_pit(X0,Y) ;
    is_pit(X,Y1) ;
    is_pit(X,Y0) ),
  retractall(signal_pit(X,Y,_)),
  assert(signal_pit(X,Y,yes)),
  !.

is_signal_pit(no) :-
  agent_location(X,Y),
  retractall(signal_pit(X,Y,_)),
  assert(signal_pit(X,Y,no)),
  !.


% is_signal_fire(Signal_fire): Signal_fire = yes if a fire is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_fire(yes) :-
  agent_location(X,Y),
  is_fire(X,Y),
  retractall(signal_fire(X,Y,_)),
  assert(signal_fire(X,Y,yes)),
  !.

is_signal_fire(no) :-
  agent_location(X,Y),
  retractall(signal_fire(X,Y,_)),
  assert(signal_fire(X,Y,no)),
  !.


% is_signal_food(Signal_food): Signal_food = yes if a food is in a square directly up, down,
%   left, or right of the current agent location.

is_signal_food(yes) :-
  agent_location(X,Y),
  is_food(X,Y),
  retractall(signal_food(X,Y,_)),
  assert(signal_food(X,Y,yes)),
  !.

is_signal_food(no) :-
  agent_location(X,Y),
  retractall(signal_food(X,Y,_)),
  assert(signal_food(X,Y,no)),
  !.


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
	agent_dies(X,Y);
	is_pit(X,Y);
	agent_time_to_starve(0);
	agent_time_to_freeze(0)
  ),
  !,
  retract(agent_health(alive)),
  assert(agent_health(dead)),
  retract(agent_score(S)),
  S1 is S - 10000,
  assert(agent_score(S1)),
  format("Você acabou de morrer!~n",[]).

update_agent_health.


% get_weapon: adds weapon to agents "inventory" if any weapon in the square

get_weapon :-
  agent_location(X,Y),
  is_weapon(X,Y), !,                   % there's a weapon in this square!
  agent_weapon(NWeapon),              %   add to agents "inventory"
  NWeapon1 is NWeapon + 1,
  retract(agent_weapon(NWeapon)),
  assert(agent_weapon(NWeapon1)),
  format("Você agora possui ~d item(ns) de arma!~n",NWeapon1),
  retract(weapon(X,Y,_)).             %   delete weapon(X,Y) from square

 get_weapon.

% get_food: adds food to agents inventory if any food in the square

get_food :-
  agent_location(X,Y),
  is_food(X,Y), !,                   % there's a food in this square!
  agent_time_to_starve(Time),              %   add to agents loot
  default_time_to_starve(ExtraTime),
  Time1 is Time + ExtraTime,
  retract(agent_time_to_starve(Time)),
  assert(agent_time_to_starve(Time1)),
  format("Você agora tem ~d ações(grab, goforward) até morrer de fome!~n",Time1),
  retract(food(X,Y,_)).             %   delete food(X,Y) from square

 get_food.

% get_fire: adds fire to agents inventory if any fire in the square

get_fire :-
  agent_location(X,Y),
  is_fire(X,Y), !,                   % there's a fire in this square!
  agent_time_to_freeze(Time),              %   add to agents loot
  default_time_to_freeze(ExtraTime),
  Time1 is Time + ExtraTime,
  retract(agent_time_to_freeze(Time)),
  assert(agent_time_to_freeze(Time1)),
  format("Você agora tem ~d ações(grab, goforward) até morrer de frio!~n",Time1),
  retractall(fire(X,Y,_)).             %   delete fire(X,Y) from square

 get_fire.

% agent_dies(X,Y)

agent_dies(X,Y) :-
  (is_enemy_tribe(X,Y), \+agent_survive_enemy_tribe);
  (is_enemy(X,Y), \+agent_survive_enemy);
  (is_wolf(X,Y), \+agent_survive_wolf).

agent_survive_enemy_tribe :-
  agent_type(tribe),
  \+agent_weapon(0).

agent_survive_enemy :-
  agent_type(tribe);
  \+agent_weapon(0).

agent_survive_wolf :-
  agent_type(tribe);
  \+agent_weapon(0).

% agent_kills

agent_kills(X,Y) :-
  ( (is_enemy_tribe(X,Y),agent_survive_enemy_tribe) *-> kill(X,Y,enemy_tribe) ; true ),
  ( (is_enemy(X,Y),agent_survive_enemy) *-> kill(X,Y,enemy) ; true ),
  ( (is_wolf(X,Y),agent_survive_wolf) *-> kill(X,Y,wolf) ; true ).


% kill(X,Y,EnemyType)

kill(X,Y,EnemyType) :-
  Enemy =.. [EnemyType,X,Y,yes],
  retractall(Enemy),
  NewEnemy =.. [EnemyType,X,Y,no],
  assert(NewEnemy).


% display_world: Displays everything known about the wumpus world,

display_world :-
  nl,
  world_extent(E),
  display_rows(E,E),
  agent_orientation(AA),
  agent_type(AT),
  agent_health(AH),
  agent_time_to_freeze(ATF),
  agent_time_to_starve(ATS),
  agent_weapon(N),
  format('Orientação do agente: ~d graus~n',[AA]),
  format('Tipo do agente: ~w~n',[AT]),
  format('Saúde do agente: ~w~n',[AH]),
  format('O agente morrerá de frio em: ~d ações(grab, goforward)~n',[ATF]),
  format('O agente morrerá de fome em: ~d ações(grab, goforward)~n',[ATS]),
  format('Número de itens arma que o agente possui: ~d~n',[N]),
  format("~nLegenda:"),
  format("
    T = Tribo de Murlocks Malvados    signal: t
    E = Murlocks Inimigos Solitarios  signal: e
    W = Lobo                          signal: w
    X = Arma                          signal: x
    P = Buraco                        signal: p
    F = Fire                          signal: f
    Q = Comida                        signal: q
  ").

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
  map_type(Type),
  atomic_concat(display_, Type, Display),
  Do =.. [Display|[X,Y]],
  Do,
  X1 is X + 1,
  display_square(X1,Y,E).

display_info(X,Y) :-
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_known_fact(enemy_tribe,X,Y,'T'),
  display_known_fact(enemy,X,Y,'E'),
  display_known_fact(wolf,X,Y,'W'),
  display_known_fact(weapon,X,Y,'X'),
  display_known_fact(pit,X,Y,'P'),
  display_known_fact(fire,X,Y,'F'),
  display_known_fact(food,X,Y,'Q'),
  write(' ').

display_location_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y],
  Fact,
  !,
  format('~w',[Atom]).

display_location_fact(_,_,_,_) :-
  format(' ',[]).

display_signals(X,Y) :-
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_known_fact(signal_enemy_tribe,X,Y,'t'),
  display_known_fact(signal_enemy,X,Y,'e'),
  display_known_fact(signal_wolf,X,Y,'w'),
  display_known_fact(signal_weapon,X,Y,'x'),
  display_known_fact(signal_pit,X,Y,'p'),
  display_known_fact(signal_fire,X,Y,'f'),
  display_known_fact(signal_food,X,Y,'q'),
  write(' ').

display_known_fact(Functor,X,Y,Atom) :-
  Fact =.. [Functor,X,Y,yes],
  Fact,
  !,
  format('~w',[Atom]).

display_known_fact(_,_,_,_) :-
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
  format("~nExecutando: ~w~n",[Action]),
  display_world.
