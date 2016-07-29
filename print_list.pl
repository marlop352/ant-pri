% print_list(List): print the list List
print_list(List) :-
  format('~n~nLista = [',[]),
  print_list_itens(List),
  format(']~n~n',[]).

print_list_itens([Fact|List]) :-
  format("~w",[Fact]),
  print_list_item(List).

print_list_item([]).

print_list_item([Fact|List]) :-
  format(", ~w",[Fact]),
  print_list_item(List).