% Math code:
%
%   abs(X,Y): Y is the absolute value of X.
%   sqrt(X,Y): Y is the square root of X.
%   exp(X,Y): Y is e^X.
%   log(X,Y): Y is logarithm base e of X (X >= 0).
%   log2(X,Y): Y is logarithm base 2 of X (X >= 0).
%   log10(X,Y): Y is logarithm base 10 of X (X >=0).
%   cos(X,Y): Y is the cosine of X (X in radians).
%   sin(X,Y): Y is the sine of X (X in radians).
%   arctan(X,Y,Z): Z (-pi < Z <= pi radians) is inverse tangent of the
%                  point (X,Y) (X and Y are any real number).

pi(3.141592654).
sqrt_precision(1.0E-10).  % decimal-place precision of squareroot
exp_series_limit(50).     % Limit on terms of exp series.
log_series_limit(99).     % Limit on terms of log series.
arctan_series_limit(99).  % Limit on terms of tan series.
sin_series_limit(50).     % Limit on terms of sin series.
cos_series_limit(99).     % Limit on terms of cos series.


% abs(X,Y): Y is the absolute value of X.

abs(X,X) :-
  X >= 0.

abs(X,Y) :-
  X < 0,
  Y is - X.


% sqrt(X,Y): Y is the square root of X.
     
sqrt(X,Y) :-
  sqrt1(X,Y,1.0).

sqrt1(X,Z,Z) :-
  W is ((Z * Z) - X),
  abs(W,AW),
  sqrt_precision(P),
  AW < P,
  !.

sqrt1(X,Y,Z) :-
  Z1 is (Z - (((Z * Z) - X) / (2 * Z))),
  sqrt1(X,Y,Z1).


% exp(X,Y): Y is e^X.

exp(X,Y) :-
  X1 is float(X),
  exp_series(X1,1,Y1,1),
  Y is Y1 + 1.0.


% exp_series(X,Term,Y,Exp): Y is e raised to the X power according
%   to the series:
%
%   e^X = 1 + X + X^2/2! + X^3/3! + X^4/4! + ...
%
%      where (2,3,4,...) continues until exceeding exp_series_limit.

exp_series(_,_,0,Exp) :-
  exp_series_limit(L),
  Exp > L,
  !.

exp_series(X,Term,Y,Exp) :-
  Term1 is Term * X / Exp,
  Exp2 is Exp + 1,
  exp_series(X,Term1,Y2,Exp2),
  Y is Term1 + Y2.


% log2(X,Y): Y is log base 2 of X.

log2(X,Y) :-
  log(X,Ye),
  Y is Ye / 0.69314718.  % log_e(2) = 0.69314718


% log10(X,Y): Y is log base 10 of X.

log10(X,Y) :-
  log(X,Ye),
  Y is Ye / 2.302585093.  % log_e(10) = 2.302585093


% log(X,Y): Y is the natural logarithm (base e) of X.

log(0,0).

log(X,Y) :-
  X > 0,
  log_series_limit(L),
  log_e_series(X,1,Y,1,L).


% log_e_series(X,Term,Y,Exp,Limit): Y is the natural logarithm (base e)
%   of X (X >= 0) according to the logarithmic series:
%
%   log_e (X) = 2 [ Term + (1/3)*Term^3 + (1/5)*Term^5 + ... +
%                          (1/Exp)*Term^Exp ]
%
%      where Term = ((X-1)/(X+1)) and Exp is the largest odd <= Limit.

log_e_series(_,_,0,Exp,Limit) :-
  Exp > Limit,
  !.

log_e_series(X,_,Y,1,Limit) :-
  !,
  Term1 is ((X-1)/(X+1)),
  Term2 is Term1 * 2,
  log_e_series(X,Term1,Y2,3,Limit),
  Y is Term2 + Y2.

log_e_series(X,Term,Y,Exp,Limit) :-
  Term1 is Term * ((X-1)/(X+1)) * ((X-1)/(X+1)),
  Term2 is Term1 * (2 / Exp),
  Exp2 is Exp + 2,
  log_e_series(X,Term1,Y2,Exp2,Limit),
  Y is Term2 + Y2.


% cos(X,Y): Y is the cosine of X (X in radians).

cos(X,Y) :-
  cos_series(X,Y,1).


% cos_series(X,Y,Base): Expands the following series to compute Y = cos(X):
%
%   cos(X) = (1 - 4X^2/Pi^2)*(1 - 4x^2/(3Pi)^2)*(1 - 4x^2/(5Pi)^2)*...
%
%     until the Base (1,3,5,...) exceeds cos_series_limit(L).

cos_series(_,1.0,Base) :-
  cos_series_limit(Limit),
  Base > Limit,
  !.

cos_series(X,Y,Base) :-
  pi(Pi),
  Term1 is (2.0 * X) / (Base * Pi),
  Term2 is 1.0 - (Term1 * Term1),
  Base1 is Base + 2,
  cos_series(X,Y1,Base1),
  Y is Term2 * Y1.


% sin(X,Y): Y is the sine of X (X in radians).

sin(X,Y) :-
  sin_series(X,Y,1).


% sin_series(X,Y,Base): Expands the following series to compute Y = sin(X):
%
%   sin(X) = (1 - X^2/Pi^2)*(1 - x^2/(2Pi)^2)*(1 - x^2/(3Pi)^2)*...
%
%     until the Base (1,2,3,...) exceeds sin_series_limit(L).

sin_series(X,X,Base) :-
  sin_series_limit(Limit),
  Base > Limit,
  !.

sin_series(X,Y,Base) :-
  pi(Pi),
  Term1 is X / (Base * Pi),
  Term2 is 1.0 - (Term1 * Term1),
  Base1 is Base + 1,
  sin_series(X,Y1,Base1),
  Y is Term2 * Y1.


% arctan(X,Y,Z): Z (-pi < Z <= pi radians) is inverse tangent of the
%   point (X,Y) (X and Y are any real number).


arctan(X,Y,Z) :-
  FX is float(X),
  FY is float(Y),
  arctan1(FX,FY,Z).


% arctan1(X,Y,Z) :- Computes Z as the inverse tangent at (X,Y) based on the
%   quadrant of (X,Y) and their ratio.

arctan1(0.0,Y,Z) :-
  Y > 0.0,
  !,
  pi(Pi),
  Z is Pi / 2.0.

arctan1(0.0,Y,Z) :-
  Y < 0.0,
  !,
  pi(Pi),
  Z is - (Pi / 2.0).

arctan1(X,0.0,0.0) :-
  X >= 0.0,
  !.

arctan1(X,0.0,Z) :-
  X < 0.0,
  !,
  pi(Z).

arctan1(X,Y,Z) :-
  X > 0.0,          % 1st quadrant
  Y > 0.0,
  X >= Y,           % angle =< 45 degrees
  !,
  Ratio is Y / X,
  arctan2(Ratio,Z).

arctan1(X,Y,Z) :-
  X > 0.0,          % 1st quadrant
  Y > 0.0,
  X < Y,            % angle > 45 degrees
  Ratio is X / Y,
  !,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is (Pi / 2.0) - Z1.

arctan1(X,Y,Z) :-
  X < 0.0,          % 2nd quadrant
  Y > 0.0,
  Y =< - X,         % angle >= 135 degrees
  !,
  Ratio is - Y / X,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is Pi - Z1.

arctan1(X,Y,Z) :-
  X < 0.0,          % 2nd quadrant
  Y > 0.0,
  Y > - X,          % angle < 135 degrees
  !,
  Ratio is - X / Y,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is (Pi / 2.0) + Z1.

arctan1(X,Y,Z) :-
  X < 0.0,          % 3rd quadrant
  Y < 0.0,
  X =< Y,           % angle =< 225 degrees
  !,
  Ratio is Y / X,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is Z1 - Pi.

arctan1(X,Y,Z) :-
  X < 0.0,          % 3rd quadrant
  Y < 0.0,
  X > Y,            % angle > 225 degrees
  !,
  Ratio is X / Y,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is - (Pi / 2.0) - Z1.

arctan1(X,Y,Z) :-
  X > 0.0,          % 4th quadrant
  Y < 0.0,
  X >= - Y,         % angle >= 315 degrees
  !,
  Ratio is - Y / X,
  arctan2(Ratio,Z1),
  Z is - Z1.

arctan1(X,Y,Z) :-
  X > 0.0,          % 4th quadrant
  Y < 0.0,
  X < - Y,          % angle < 315 degrees
  !,
  Ratio is - X / Y,
  arctan2(Ratio,Z1),
  pi(Pi),
  Z is Z1 - (Pi / 2.0).


% arctan2(X,Y): Y is the inverse tangent of X, where 0.0 < X <= 1.0.

arctan2(1.0,Y) :-
  !,
  pi(Pi),
  Y is Pi / 4.0.

arctan2(X,Y) :-         % 0.0 < X < 1.0
  arctan_series_limit(L),
  arctan_series(X,X,Y,1,1,L).


% arctan_series(X,Term,Y,Sign,Exp,Limit): Y is the inverse tangent
%   of X (0.0 < X < 1.0) according to the series:
%
%   arctan(X) = X - X^3/3 + X^5/5 - X^7/7 + X^9/9 - ... +/-
%                          (1/Exp)*X^Exp ]
%
%      where Exp is the largest odd <= Limit.


arctan_series(_,_,0,_,Exp,Limit) :-
  Exp > Limit,
  !.

arctan_series(X,Term,Y,Sign,Exp,Limit) :-
  Term1 is Term * X * X,
  Term2 is Sign * (Term / Exp),
  Exp2 is Exp + 2,
  OppSign is Sign * (-1),
  arctan_series(X,Term1,Y2,OppSign,Exp2,Limit),
  Y is Term2 + Y2.
