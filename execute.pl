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
  format("You have left the cave.~n",[]).

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
  format("You cannot leave the cave from here.~n",[]).
