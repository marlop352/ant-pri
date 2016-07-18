% Rule-based movement for the wumpus
%
% Based on a randomly-selected rule (but fixed for each randomly-generated
% wumpus world), the wumpus can move after each call to the execute(Act,Pcpt)
% procedure.  First, the agent's move is processed, then the wumpus moves
% (and may move into your square), and then the new percept is generated and
% returned.  The wumpus can execute the actions: nil, goforward, turnleft or
% turnright.  The 'nil' action does nothing.  The rules are described in
% individual comments near the end of this file.
%
% Note: The wumpus is not affected by pits.
%       The wumpus will never enter location (1,1).
%
% Loaded from: wumpus2.pl


% move_wumpus: Moves the wumpus according to a pre-selected Rule as defined
%   by wumpus_movement_rule(Rule).

move_wumpus :-
  wumpus_health(dead),
  !.

move_wumpus :-
  wumpus_health(alive),
  wumpus_movement_rule(Rule),
  wumpus_movement_rule(Rule,Action),
  execute_wumpus_action(Action),
  retract(wumpus_last_action(_)),
  assert(wumpus_last_action(Action)).


% execute_wumpus_action(Action):  Similar to agent movement, where Action is
%   one of goforward, turnleft or turnright.  Wumpus cannot goforward into
%   location (1,1).  If goforward succeeds, then check agent's health.

execute_wumpus_action(goforward) :-
  wumpus_orientation(Angle),
  wumpus_location(X,Y),
  new_location(X,Y,Angle,X1,Y1),
  wumpus_world_extent(E),          % check if off world
  X1 > 0,
  X1 =< E,
  Y1 > 0,
  Y1 =< E,
  ( X1 > 1 ; Y1 > 1 ),             % can't go into 1,1
  !,
  retract(wumpus_location(X,Y)),   % update location
  assert(wumpus_location(X1,Y1)),
  update_agent_health.

execute_wumpus_action(goforward).  % unsuccessfully

execute_wumpus_action(turnleft) :-
  retract(wumpus_orientation(Angle)),
  NewAngle is (Angle + 90) mod 360,
  assert(wumpus_orientation(NewAngle)).

execute_wumpus_action(turnright) :-
  retract(wumpus_orientation(Angle)),
  NewAngle is (Angle + 270) mod 360,
  assert(wumpus_orientation(NewAngle)).

execute_wumpus_action(nil).


% List of names of available wumpus movement rules.

wumpus_movement_rules([
  sit,
  spin,
  random,
  bump_turn,
  loop,
  goto_gold,
  goto_pit
  ]).

% wumpus_movement_rule(Rule,Action):  Uses wumpus movement Rule to
%   determine the next action for the wumpus (goforward, turnleft or
%   turnright).  When adding new rules, be sure to add their rule names
%   to the  wumpus_movement_rules list above.


%%% sit

wumpus_movement_rule(sit,nil).


%%% spin

wumpus_movement_rule(spin,turnleft).


%%% random

wumpus_movement_rule(random,Action) :-
  random_member(Action,[nil,turnleft,turnright,goforward]).


%%% bump_turn

wumpus_movement_rule(bump_turn,Action) :-
  wumpus_location(X,Y),
  wumpus_orientation(Orient),
  ( facing_wall(X,Y,Orient) ;
    facing_home(X,Y,Orient) ),
  !,
  random_member(Action,[turnleft,turnright]).

wumpus_movement_rule(bump_turn,goforward).


%%% loop

wumpus_movement_rule(loop,turnleft) :-
  wumpus_last_action(goforward),
  !.

wumpus_movement_rule(loop,goforward).


%%% goto_gold

wumpus_movement_rule(goto_gold,nil) :-    % no gold
  not gold(_,_),
  !.

wumpus_movement_rule(goto_gold,nil) :-    % already next to gold
  gold(GX,GY),
  wumpus_location(WX,WY),
  adjacent(WX,WY,GX,GY),
  !.

wumpus_movement_rule(goto_gold,Action) :- % move towards gold
  gold(GX,GY),
  !,           % prevent backtracking on other gold pieces
  wumpus_location(WX,WY),
  wumpus_orientation(Orient),
  move_towards(WX,WY,Orient,GX,GY,Action).


%%% goto_pit

wumpus_movement_rule(goto_pit,nil) :-    % no pits
  not pit(_,_),
  !.

wumpus_movement_rule(goto_pit,nil) :-    % already next to a pit
  pit(PX,PY),
  wumpus_location(WX,WY),
  adjacent(PX,PY,WX,WY),
  !.

wumpus_movement_rule(goto_pit,Action) :- % move towards a pit
  pit(PX,PY),
  !,           % prevent backtracking on other pits
  wumpus_location(WX,WY),
  wumpus_orientation(Orient),
  move_towards(WX,WY,Orient,PX,PY,Action).


% facing_wall(X,Y,Orient): True if location (X,Y) is next to a wall, and
%   Orient is facing wall.

facing_wall(1,_,180).

facing_wall(_,1,270).

facing_wall(X,_,0) :-
  wumpus_world_extent(X).

facing_wall(_,Y,90) :-
  wumpus_world_extent(Y).


% facing_home(X,Y,Orient):  True if location (X,Y) is next to (1,1), and
%   Orient is facing (1,1).

facing_home(1,2,270).

facing_home(2,1,180).


% adjacent(X1,Y1,X2,Y2): True if location (X1,Y1) is next to (X2,Y2).

adjacent(X1,Y1,X2,Y2) :-
  ( X1 =:= X2, Y1 is Y2 + 1 ;
    X1 =:= X2, Y1 is Y2 - 1 ;
    Y1 =:= Y2, X1 is X2 + 1 ;
    Y1 =:= Y2, X1 is X2 - 1 ).


% move_towards(X1,Y1,Orient,X2,Y2,Action): Assuming (X1,Y1) is not adjacent
%   to (X2,Y2), Action (turnleft, turnright or gofoward) is chosen so
%   as to move from (X1,Y1), in direction Orient, towards (X2,Y2).

move_towards(X1,Y1,Orient,X2,Y2,Action) :-
  DX is X2 - X1,
  DY is Y2 - Y1,
  arctan(DX,DY,AngleRad),
  radians_to_degrees(AngleRad,Angle),
  nearest_orientation(Angle,Orient2),
  direction_action(Orient,Orient2,Action).


% radians_to_degrees(AngleRad,AngleDeg):  AngleDeg is the integer angle (in
%   degrees) equivalent to AngleRad (in radians), where -Pi < AngleRad <= Pi
%   and 0 <= AngleDeg < 360.

radians_to_degrees(AngleRad,AngleDeg) :-
  AngleRad < 0.0,
  !,
  pi(Pi),
  AngleRadP is (2.0 * Pi) + AngleRad,
  AngleDeg1 is AngleRadP * (180.0 / Pi),
  AngleDeg is integer(AngleDeg1).

radians_to_degrees(AngleRad,AngleDeg) :-
  pi(Pi),
  AngleDeg1 is AngleRad * (180.0 / Pi),
  AngleDeg is integer(AngleDeg1).


% nearest_orientation(Angle,Orient):  Orient is the nearest orientation
%   (0,90,180,270) to Angle (in degrees), where 0 <= Angle < 360.

nearest_orientation(Angle,0) :-
  ( Angle =< 45 ;
    Angle > 315 ),
  !.

nearest_orientation(Angle,90) :-
  Angle > 45,
  Angle =< 135,
  !.

nearest_orientation(Angle,180) :-
  Angle > 135,
  Angle =< 225,
  !.

nearest_orientation(_,270).


% direction_action(Orient1,Orient2,Action): Action = goforward if
%   Orient1=Orient2.  Otherwise, Action is turnleft or turnright
%   according to the difference between Orient1 and Orient2.

direction_action(270,0,turnleft) :- !.

direction_action(0,270,turnright) :- !.

direction_action(Orient1,Orient2,turnleft) :-
  Orient1 < Orient2,
  !.

direction_action(Orient1,Orient2,turnright) :-
  Orient1 > Orient2,
  !.

direction_action(_,_,goforward).

