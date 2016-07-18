% cs_image.pl
%
% Larry Holder (holder@cse.uta.edu)
%
% Inserts circles and squares into a binary image.  Circles are given by
% their center (X,Y) and radius R, where X, Y and R are integers.  Squares
% are given by their center (X,Y), distance R from center to side, and
% rotation angle A (0 <= A < 90), where X, Y and S are integers.  The image
% is of size NxN as given, where N is an integer.
%
% Note: The specification of squares here is slightly different than in
%       the HW6 writeup to ensure odd side lengths and exact centers.
%
% Requires: math.pl utils.pl

:- load_files([ math, utils ]).


% cs_image(Objs,N,Image): Returns an NxN binary Image containing Objs,
%   where Objs is a list of elements of the form circle(X,Y,R) or
%   square(X,Y,S,A).

cs_image(Objs,N,Image) :-
  cs_points(Objs,Points),
  RowNum is N - 1,
  gen_image_rows(N,Points,RowNum,Image).


% cs_points(Objs,Points):  Returns a list of [X,Y] points that should be
%   on (=1) to depict the objects in Objs.

cs_points([],[]).

cs_points([circle(X,Y,R)|Objs],Points) :-
  circle_points(X,Y,R,CirclePoints),
  cs_points(Objs,Points1),
  conc(CirclePoints,Points1,Points).

cs_points([square(X,Y,R,A)|Objs],Points) :-
  square_points(X,Y,R,SquarePoints),
  degrees_to_radians(A,AR),
  rotate_points(X,Y,AR,SquarePoints,SquarePointsR),
  cs_points(Objs,Points1),
  conc(SquarePointsR,Points1,Points).


% circle_points(X,Y,R,Points): Points are all the points on the circle
%   with center (X,Y) and radius R.

circle_points(X,Y,R,Points) :-
  IStart is X - R,
  IEnd is X + R,
  scan_circle_points(IStart,IEnd,X,Y,R,Points).


% scan_circle_points(I,IEnd,X,Y,R,Points): For each value from I to IEnd,
%   determines the two possible J values such that (I,J) is on the
%   circle centered at (X,Y) with radius R according to the equation:
%     (X - I)^2 + (Y - I)^2 = R^2

scan_circle_points(I,IEnd,_,_,_,[]) :-
  I > IEnd.

scan_circle_points(I,IEnd,X,Y,R,[[I,J1],[I,J2]|Points]) :-
  I =< IEnd,
  YDiff2 is (R * R) - ((X - I) * (X - I)),
  sqrt(YDiff2,YDiff),
  YDiffI is integer(YDiff),
  J1 is Y + YDiffI,
  J2 is Y - YDiffI,
  INext is I + 1,
  scan_circle_points(INext,IEnd,X,Y,R,Points).


% square_points(X,Y,R,Points): Points are all the points on the axis-
%   aligned square centered at (X,Y) with sides of length 2R+1.

square_points(X,Y,R,Points) :-
  IStart is X - R,
  IEnd is X + R,
  JStart is Y - R,
  JEnd is Y + R,
  horizontal_points(IStart,IEnd,JStart,JEnd,HPoints),
  vertical_points(JStart,JEnd,IStart,IEnd,VPoints),
  conc(HPoints,VPoints,Points).


% horizontal_points(I,IEnd,J1,J2,HPoints): Generates the HPoints along
%   two horizontal line segments from [I,J1] and [I,J2] to [IEnd,J1]
%   and [IEnd,J2].

horizontal_points(I,IEnd,_,_,[]) :-
  I > IEnd.

horizontal_points(I,IEnd,J1,J2,[[I,J1],[I,J2]|HPoints]) :-
  I =< IEnd,
  INext is I + 1,
  horizontal_points(INext,IEnd,J1,J2,HPoints).


% vertical_points(J,JEnd,I1,I2,VPoints): Generates the VPoints along
%   two vertical line segments from [I1,J] and [I2,J] to [I1,JEnd]
%   and [I2,JEnd].

vertical_points(J,JEnd,_,_,[]) :-
  J > JEnd.

vertical_points(J,JEnd,I1,I2,[[I1,J],[I2,J]|VPoints]) :-
  J =< JEnd,
  JNext is J + 1,
  vertical_points(JNext,JEnd,I1,I2,VPoints).


% rotate_points(X,Y,A,Points1,Points2): Points2 are the points in Points1
%   rotated about (X,Y) for A radians.

rotate_points(_,_,_,[],[]).

rotate_points(X,Y,A,[[I,J]|Points1],[[IR,JR]|Points2]) :-
  cos(A,CA),
  sin(A,SA),
  IRF is X + ((I - X) * CA) - ((J - Y) * SA),
  IR is integer(IRF),
  JRF is Y + ((I - X) * SA) + ((J - Y) * CA),
  JR is integer(JRF),
  rotate_points(X,Y,A,Points1,Points2).


% degrees_to_radians(AD,AR):  AR is the radian equivalent of AD degrees.

degrees_to_radians(AD,AR) :-
  pi(Pi),
  AR is (AD / 180.0) * Pi.


% print_image(Image): Displays image using *'s for 1's and spaces for 0's.

print_image([]).

print_image([Row|Rows]) :-
  print_image_row(Row),
  nl,
  print_image(Rows).

print_image_row([]).

print_image_row([0|Cols]) :-
  tab(2),
  print_image_row(Cols).

print_image_row([1|Cols]) :-
  write('*'), tab(1),
  print_image_row(Cols).


% gen_image_rows(N,Points,RowNum,Rows):  Generates the rows of
%   binary-valued pixels by including any points in Points from
%   RowNum to 0, where RowNum < N.

gen_image_rows(_,_,RowNum,[]) :-
  RowNum < 0,
  !.

gen_image_rows(N,Points,RowNum,[Row|Rows]) :-
  gen_image_row(N,Points,RowNum,0,Row),
  RowNum1 is RowNum - 1,
  gen_image_rows(N,Points,RowNum1,Rows).


gen_image_row(N,_,_,ColNum,[]) :-
  ColNum >= N,
  !.

gen_image_row(N,Points,RowNum,ColNum,[1|Cols]) :-
  member([ColNum,RowNum],Points),
  !,
  ColNum1 is ColNum + 1,
  gen_image_row(N,Points,RowNum,ColNum1,Cols).

gen_image_row(N,Points,RowNum,ColNum,[0|Cols]) :-
  ColNum1 is ColNum + 1,
  gen_image_row(N,Points,RowNum,ColNum1,Cols).
