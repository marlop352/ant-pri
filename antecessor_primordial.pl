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

:- dynamic([				% objects stuff
  enemy_tribe/2,
  enemy/2,
  wolf/2,
  weapon/2,
  pit/2,
  fire/2,
  food/2]).

:- dynamic([				% known signals stuff
  signal_enemy_tribe_position/2,
  signal_enemy_position/2,
  signal_wolf_position/2,
  signal_weapon_position/2,
  signal_pit_position/2,
  signal_fire_position/2,
  signal_food_position/2]).

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
 
 %Code to gen map! 
:- retractall(map_type(_)),assert(map_type(info)).

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

%------------NSS------------------------

io_sig_TS(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible Tribe of Murlocks at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, '  BEWARE!!!!'), nl(Stream),
    close(Stream).

io_sig_ES(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible Murlock Fighter at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, '  BEWARE!!!!'), nl(Stream),
    close(Stream).

io_sig_H(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible Wolf at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, '  BEWARE!!!!'), nl(Stream),
    close(Stream).

io_sig_B(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible Hole at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, '  Dont fall in!'), nl(Stream),
    close(Stream).

io_sig_S(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'There might be food at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, '  Yummy!'), nl(Stream),
    close(Stream).

io_sig_F(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible fire at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, ' . Tosty...'), nl(Stream),
    close(Stream).

io_sig_X(X,Y):-
    open('dd_map.txt', append,Stream),
    write(Stream, 'Possible rifle at X: '),write(Stream, X),write(Stream, ' Y:'), write(Stream, Y), write(Stream, ' . Piu piu!'), nl(Stream),
    close(Stream).
%-------------------------------------------

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
%
%   Percept = [Stench,Signal_pit,Glitter,Bump,Scream]
%             These variables are either 'yes' or 'no'.  

execute(_,[no,no,no,no,no,no,no]) :-
  agent_health(dead), !,         % agent must be alive to execute actions
  format("You are dead!~n",[]).

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
  assert(signal_enemy_tribe_position(X,Y)),
  !.

signal_enemy_tribe(no).

% if_sigTS
if_sigT(yes) :- 
    signal_enemy_tribe(yes),
    agent_location(X,Y),
    io_sig_TS(X,Y).


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
  assert(signal_enemy_position(X,Y)),
  !.

signal_enemy(no).

if_sigE(yes) :- 
    signal_enemy(yes),
    agent_location(X,Y),
    io_sig_ES(X,Y).

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
  assert(signal_wolf_position(X,Y)),
  !.

signal_wolf(no).

if_sigW(yes) :- 
    signal_wolf(yes),
    agent_location(X,Y),
    io_sig_H(X,Y).

% signal_weapon(Signal_weapon): Signal_weapon = yes if a weapon is in a square directly up, down,
%   left, or right of the current agent location.

signal_weapon(yes) :-
  agent_location(X,Y),
  weapon(X,Y),
  assert(signal_weapon_position(X,Y)),
  !.

signal_weapon(no).

if_sigX(yes) :- 
    signal_weapon(yes),
    agent_location(X,Y),
    io_sig_X(X,Y).

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
  assert(signal_pit_position(X,Y)),
  !.

signal_pit(no).

if_sigP(yes) :- 
    signal_pit(yes),
    agent_location(X,Y),
    io_sig_B(X,Y).

% signal_fire(Signal_fire): Signal_fire = yes if a fire is in a square directly up, down,
%   left, or right of the current agent location.

signal_fire(yes) :-
  agent_location(X,Y),
  fire(X,Y),
  assert(signal_fire_position(X,Y)),
  !.

signal_fire(no).


if_sigF(yes) :- 
    signal_pit(yes),
    agent_location(X,Y),
    io_sig_F(X,Y).

% signal_food(Signal_food): Signal_food = yes if a food is in a square directly up, down,
%   left, or right of the current agent location.

signal_food(yes) :-
  agent_location(X,Y),
  food(X,Y),
  assert(signal_food_position(X,Y)),
  !.

signal_food(no).

if_sigQ(yes) :- 
    signal_food(yes),
    agent_location(X,Y),
    io_sig_S(X,Y).

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


% get_weapon: adds weapon to agents "inventory" if any weapon in the square

get_weapon :-
  agent_location(X,Y),
  weapon(X,Y), !,                   % there's a weapon in this square!
  agent_weapon(NWeapon),              %   add to agents "inventory"
  NWeapon1 is NWeapon + 1,
  retract(agent_weapon(NWeapon)),
  assert(agent_weapon(NWeapon1)),
  format("You now have ~d weapon(s)!~n",NWeapon1),
  retract(weapon(X,Y)).             %   delete weapon(X,Y) from square

 get_weapon.

% get_food: adds food to agents inventory if any food in the square

get_food :-
  agent_location(X,Y),
  food(X,Y), !,                   % there's a food in this square!
  agent_time_to_starve(Time),              %   add to agents loot
  default_time_to_starve(ExtraTime),
  Time1 is Time + ExtraTime,
  retract(agent_time_to_starve(Time)),
  assert(agent_time_to_starve(Time1)),
  format("You now have ~d round(s) until you starve!~n",Time1),
  retract(food(X,Y)).             %   delete food(X,Y) from square

 get_food.

% get_fire: adds fire to agents inventory if any fire in the square

get_fire :-
  agent_location(X,Y),
  fire(X,Y), !,                   % there's a fire in this square!
  agent_time_to_freeze(Time),              %   add to agents loot
  default_time_to_freeze(ExtraTime),
  Time1 is Time + ExtraTime,
  retract(agent_time_to_freeze(Time)),
  assert(agent_time_to_freeze(Time1)),
  format("You now have ~d round(s) until you freeze!~n",Time1),
  retract(fire(X,Y)).             %   delete fire(X,Y) from square

 get_fire.

% agent_dies(X,Y)

agent_dies(X,Y) :-
  (enemy_tribe(X,Y), \+agent_survive_enemy_tribe);
  (enemy(X,Y), \+agent_survive_enemy);
  (wolf(X,Y), \+agent_survive_wolf).

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
  ( (enemy_tribe(X,Y),agent_survive_enemy_tribe) *-> kill(X,Y,enemy_tribe) ; true ),
  ( (enemy(X,Y),agent_survive_enemy) *-> kill(X,Y,enemy) ; true ),
  ( (wolf(X,Y),agent_survive_wolf) *-> kill(X,Y,wolf) ; true ).


% kill(X,Y,EnemyType)

kill(X,Y,EnemyType) :-
  Enemy =.. [EnemyType|[X,Y]],
  retractall(Enemy).


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
  format('agent_weapon(~d)~n',[N]),
  format("~nLegenda: ~n"),
  format("T = Tribo de Murlocks Malvados  ~nE = Murlocks Inimigos Solitarios  ~nW = Lobo ~nX = Arma  ~nP = Buraco ~nF = Fire ~nQ = Comida~n").

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
  display_location_fact(enemy_tribe,X,Y,'T'),
  display_location_fact(enemy,X,Y,'E'),
  display_location_fact(wolf,X,Y,'W'),
  display_location_fact(weapon,X,Y,'X'),
  display_location_fact(pit,X,Y,'P'),
  display_location_fact(fire,X,Y,'F'),
  display_location_fact(food,X,Y,'Q'),
  write(' ').

display_signals(X,Y) :-
  agent_orientation(AO),
  display_agent(AO,AC),
  display_location_fact(agent_location,X,Y,AC),
  display_location_fact(signal_enemy_tribe_position,X,Y,'t'),
  display_location_fact(signal_enemy_position,X,Y,'e'),
  display_location_fact(signal_wolf_position,X,Y,'w'),
  display_location_fact(signal_weapon_position,X,Y,'x'),
  display_location_fact(signal_pit_position,X,Y,'p'),
  display_location_fact(signal_fire_position,X,Y,'f'),
  display_location_fact(signal_food_position,X,Y,'q'),
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


