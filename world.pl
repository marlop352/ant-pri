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