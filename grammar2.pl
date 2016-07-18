% Simple English arithmetic grammar

expr --> digit |  expr, operator, expr.

operator --> [plus] | [minus].

digit --> [0] | [1] | [2] | [3] | [4] | [5] | [6] | [7] | [8] | [9].


% Simple English arithmetic grammar with parse tree

expr(expr(E)) --> digit(E).
expr(expr(E1,Op,E2)) --> expr(E1), operator(Op), expr(E2).

operator(op(plus)) --> [plus].
operator(op(minus)) --> [minus].

digit(digit(0)) --> [0].
digit(digit(1)) --> [1].
digit(digit(2)) --> [2].
digit(digit(3)) --> [3].
digit(digit(4)) --> [4].
digit(digit(5)) --> [5].
digit(digit(6)) --> [6].
digit(digit(7)) --> [7].
digit(digit(8)) --> [8].
digit(digit(9)) --> [9].


% Semantic evaluation of arithmetic parse tree.
% eval(ParseTree,Result)

eval(expr(digit(D)),D).

eval(expr(E1,op(Op),E2),R) :-
  eval(E1,R1),
  eval(E2,R2),
  combine(R1,Op,R2,R).

combine(R1,plus,R2,R) :-
  R is R1 + R2.

combine(R1,minus,R2,R) :-
  R is R1 - R2.


% Arithmetic DCG grammar with semantics added

expr1(R) --> digit1(R).
expr1(R) --> expr1(R1), operator1(Op), expr1(R2), {combine(R1,Op,R2,R)}.

operator1(plus) --> [plus].
operator1(minus) --> [minus].

digit1(0) --> [0].
digit1(1) --> [1].
digit1(2) --> [2].
digit1(3) --> [3].
digit1(4) --> [4].
digit1(5) --> [5].
digit1(6) --> [6].
digit1(7) --> [7].
digit1(8) --> [8].
digit1(9) --> [9].
