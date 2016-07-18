% Some simple test agents.
%
% To define an agent within the navigate.pl scenario, define:
%   init_agent
%   restart_agent
%   run_agent
%
% Currently set up to solve the wumpus world in Figure 6.2 of Russell and
% Norvig.  You can enforce generation of this world by changing the
% initialize(random,Percept) to initialize(fig62,Percept) in the
% navigate(Actions,Score,Time) procedure in file navigate.pl and then run
% navigate(Actions,Score,Time).

:- dynamic([fig62acts/1]).


init_agent :-
  init_agent_fig62.

init_agent_fig62 :-
  retractall(fig62acts(_)),
  assert(fig62acts([goforward,turnleft,goforward,goforward,grab,
                    turnleft,turnleft,goforward,goforward,turnright,
                    goforward,climb])).


restart_agent :-
  init_agent_fig62.


run_agent(Percept,Action) :-
  fig62_agent(Percept,Action),
  display_world.

fig62_agent(_,Action) :-
  retract(fig62acts([Action|Actions])),
  assert(fig62acts(Actions)).


% Other agents

headstrong_agent(_,goforward).

random_agent(_,Action) :-
  random_member(Action,[goforward,turnleft,turnright,grab,shoot,climb]).
