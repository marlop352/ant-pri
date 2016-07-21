initialize_world(random) :-
  wumpus_world_default_extent(WWS),
  ww_retractall,
  retractall(ww_initial_state(_)),
  assert(ww_initial_state([])),
  addto_ww_init_state(wumpus_world_extent(WWS)),
  all_squares(WWS,AllSqrs),
  delete(AllSqrs,[1,1],AllSqrs1),   % deletes element [1,1] from list AllSqrs
  random_member([WX,WY],AllSqrs1),  % initialize wumpus
  addto_ww_init_state(wumpus_location(WX,WY)),
  addto_ww_init_state(wumpus_health(alive)),
  gold_probability(PG),             % place gold
  place_objects(gold,PG,AllSqrs,GRestSqrs),
  at_least_one_gold(WWS,AllSqrs,GRestSqrs,PSqrs),
  delete(PSqrs,[1,1],PSqrs1),       % deletes element [1,1] from list PSqrs
  delete(PSqrs1,[WX,WY],PSqrs2),    % deletes element [WX,WY] from list PSqrs1
  pit_probability(PP),              % place pits
  place_objects(pit,PP,PSqrs2,_),
  
  ww_initial_state(L),
  assert_list(L).
