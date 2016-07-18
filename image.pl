% Image generator for wumpus world simulator.
%
% Based on the agent's current location and orientation, generate an image
% of what's in the square the agent is facing.  The image will be binary,
% NxN as controlled by image_dimen(N).
%
% If the agent is facing a wall, then the image is all ones.
% If the agent is facing an empty square, the image is all zeros.
% If there is gold, a pit, or a live wumpus in the facing square,
% then the appropriate bitmap is placed somewhere in the image rotated by
% 0, 90, 180 or 270 degrees.  The bitmaps will always be non-overlapping.
%
% See the comments on the bitmaps at the end of this file for details on the
% bitmaps (e.g., gold = trapezoid, pit = ellipse, wumpus = parabola).
%
% Loaded from: wumpus2.pl


image_dimen(25).     % Dimension of square image (NxN).

% Placement of bitmaps within a 25x25 binary image, depending on which
% items are present in the scene.
%   1. Randomly choose orientation (0,90,180,270) of gold.
%   2. Randomly choose side (top,bottom,left,right) for gold.
%   3. Place gold.
%   4. Choose orientation for pit perpendicular to gold.
%   5. Randomly choose side for pit.
%   6. Place pit.
%   7. Place wumpus.

generate_image(Image) :-
  agent_location(X,Y),
  agent_orientation(Orient),
  new_location(X,Y,Orient,X1,Y1),
  place_gold_bitmap(X1,Y1,GSide,GoldPoints),
  place_pit_bitmap(X1,Y1,GSide,PSide,PitPoints),
  place_wumpus_bitmap(X1,Y1,GSide,PSide,WumpusPoints),
  conc(GoldPoints,PitPoints,L1),
  conc(WumpusPoints,L1,L2),
  generate_points(X1,Y1,L2,Image).


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


% generate_points(X,Y,Points,Image):  If (X,Y) is off world, then all pixels
%   are black (1).  Otherwise, every point in Points is set to black (1) in
%   the image, and all others are white (0).

generate_points(X,Y,Points,Image) :-
  detect_wall(X,Y,Wall),
  image_dimen(MaxRow),
  generate_image_rows(Wall,Points,MaxRow,Image).


detect_wall(X,Y,true) :-
  wumpus_world_extent(E),
  ( X < 1 ;
    Y < 1 ;
    X > E ;
    Y > E ),
  !.

detect_wall(_,_,false).


% generate_image_rows(Wall,Points,RowNum,Rows):  Generates the rows of
%   binary-valued pixels by including any points in Points or a row of
%   all 1's if Wall=true.

generate_image_rows(_,_,0,[]) :- !.

generate_image_rows(Wall,Points,RowNum,[Row|Rows]) :-
  generate_image_row(Wall,Points,RowNum,1,Row),
  RowNum1 is RowNum - 1,
  generate_image_rows(Wall,Points,RowNum1,Rows).


generate_image_row(_,_,_,ColNum,[]) :-
  image_dimen(MaxCol),
  ColNum > MaxCol,
  !.

generate_image_row(true,_,_,ColNum,[1|Cols]) :-
  ColNum1 is ColNum + 1,
  generate_image_row(true,_,_,ColNum1,Cols).

generate_image_row(false,Points,RowNum,ColNum,[1|Cols]) :-
  member([ColNum,RowNum],Points),
  !,
  ColNum1 is ColNum + 1,
  generate_image_row(false,Points,RowNum,ColNum1,Cols).

generate_image_row(false,Points,RowNum,ColNum,[0|Cols]) :-
  ColNum1 is ColNum + 1,
  generate_image_row(false,Points,RowNum,ColNum1,Cols).


% place_gold_bitmap(X,Y,GSide,GoldPoints): Choose a random side GSide
%  of the image to put the gold bitmap and put the corresponding points
%  into GoldPoints if there really is gold at location (X,Y).

place_gold_bitmap(X,Y,GSide,GoldPoints) :-
  random_member(GOrient,[0,90,180,270]),
  gold_side(GOrient,GSide),
  gold_points(X,Y,GOrient,GSide,GoldPoints).


% gold_side(GOrient,GSide): Randomly choose a side GSide of the image to
%   place the gold bitmap according to its orientation GOrient.  Note that
%   elongated side of the gold bitmap must be placed parallel to the side.

gold_side(GOrient,GSide) :-
  ( GOrient = 0 ;
    GOrient = 180 ),
  random_member(GSide,[top,bottom]).

gold_side(GOrient,GSide) :-
  ( GOrient = 90 ;
    GOrient = 270 ),
  random_member(GSide,[left,right]).


% gold_points(X,Y,GOrient,GSide,GoldPoints): If there's gold at (X,Y), then
%   randomly choose an origin for the gold bitmap (subject to non-overlap
%   constraints) and translate the points.  If no gold, then GoldPoints = [].

gold_points(X,Y,GOrient,GSide,GoldPoints) :-
  gold(X,Y), !,
  gold_origin(GSide,GX,GY),
  gold_bitmap(GOrient,Points),
  translate_points(GX,GY,Points,GoldPoints).

gold_points(_,_,_,_,[]).


% gold_origin(GSide,GX,GY): (GX,GY) randomly chosen as the origin of
%   the gold bitmap.  The bitmap will be placed beginning at (GX+1,GY+1).

gold_origin(top,GX,GY) :-
  random(1,11,GX),
  random(17,20,GY).

gold_origin(bottom,GX,GY) :-
  random(1,11,GX),
  random(1,4,GY).

gold_origin(left,GX,GY) :-
  random(1,4,GX),
  random(1,11,GY).

gold_origin(right,GX,GY) :-
  random(17,20,GX),
  random(1,11,GY).


% place_pit_bitmaps(X,Y,GSide,PSide,PitPoints):  If a pit resides at location
%   (X,Y), then PitPoints will be a list of points placing a pit bitmap in the
%   image on PSide perpendicular to GSide.

place_pit_bitmap(X,Y,GSide,PSide,PitPoints) :-
  pit_side(GSide,POrient,PSide),
  pit_points(X,Y,GSide,POrient,PSide,PitPoints).


% pit_side(GSide,POrient,PSide): Randomly choose an orientation POrient and
%   image side PSide to place the pit bitmap such that the elongated side
%   of the pit bitmap is perpendicular to the gold bitmap at GSide.

pit_side(GSide,POrient,PSide) :-
  ( GSide = top ;
    GSide = bottom ),
  random_member(POrient,[90,270]),
  random_member(PSide,[left,right]).

pit_side(GSide,POrient,PSide) :-
  ( GSide = left ;
    GSide = right ),
  random_member(POrient,[0,180]),
  random_member(PSide,[top,bottom]).


% pit_points(X,Y,GSide,POrient,PSide,PitPoints): If there's a pit at (X,Y),
%   then randomly choose an origin for the pit bitmap (subject to non-overlap
%   constraints) and translate the points.  If no pit, then PitPoints = [].

pit_points(X,Y,GSide,POrient,PSide,PitPoints) :-
  pit(X,Y), !,
  pit_origin(GSide,PSide,PX,PY),
  pit_bitmap(POrient,Points),
  translate_points(PX,PY,Points,PitPoints).

pit_points(_,_,_,_,_,[]).


% pit_origin(GSide,PSide,PX,PY): (PX,PY) randomly chosen as the origin of
%   the pit bitmap subject to non-overlap and ortho-gold constraints.  The
%   bitmap will be placed beginning at (PX+1,PY+1).

pit_origin(top,left,PX,PY) :-
  random(1,4,PX),
  random(1,4,PY).

pit_origin(top,right,PX,PY) :-
  random(15,18,PX),
  random(1,4,PY).

pit_origin(bottom,left,PX,PY) :-
  random(1,4,PX),
  random(9,12,PY).

pit_origin(bottom,right,PX,PY) :-
  random(15,18,PX),
  random(9,12,PY).

pit_origin(left,top,PX,PY) :-
  random(9,12,PX),
  random(15,18,PY).

pit_origin(left,bottom,PX,PY) :-
  random(9,12,PX),
  random(1,4,PY).

pit_origin(right,top,PX,PY) :-
  random(1,4,PX),
  random(15,18,PY).

pit_origin(right,bottom,PX,PY) :-
  random(1,4,PX),
  random(1,4,PY).


% place_wumpus_bitmaps(X,Y,GSide,PSide,WumpusPoints):  If a live wumpus
%   at location (X,Y), then WumpusPoints will be a list of points placing
%   a wumpus bitmap in the image at a position not overlapping the gold
%   (GSide) and pit (PSide) bitmaps (whether there or not).

place_wumpus_bitmap(X,Y,GSide,PSide,WumpusPoints) :-
  wumpus_health(alive),
  wumpus_location(X,Y), !,
  random_member(WOrient,[0,90,180,270]),
  wumpus_origin(GSide,PSide,WX,WY),
  wumpus_bitmap(WOrient,Points),
  translate_points(WX,WY,Points,WumpusPoints).

place_wumpus_bitmap(_,_,_,_,[]).


% wumpus_origin(GSide,PSide,WX,WY): (WX,WY) randomly chosen as the origin
%   of the wumpus bitmap subject to non-overlap with gold (GSide) and pit
%   (PSide).  The bitmap will be placed beginning at (WX+1,WY+1).

wumpus_origin(top,left,WX,WY) :-
  random(10,13,WX),
  random(1,5,WY).

wumpus_origin(top,right,WX,WY) :-
  random(1,4,WX),
  random(1,5,WY).

wumpus_origin(bottom,left,WX,WY) :-
  random(10,13,WX),
  random(9,13,WY).

wumpus_origin(bottom,right,WX,WY) :-
  random(1,4,WX),
  random(9,13,WY).

wumpus_origin(left,top,WX,WY) :-
  random(9,13,WX),
  random(1,4,WY).

wumpus_origin(left,bottom,WX,WY) :-
  random(9,13,WX),
  random(10,13,WY).

wumpus_origin(right,top,WX,WY) :-
  random(1,5,WX),
  random(1,4,WY).

wumpus_origin(right,bottom,WX,WY) :-
  random(1,5,WX),
  random(10,13,WY).


% translate_points(X,Y,Points,TransPoints):  TransPoints is Points where
%   each point is offset by (X,Y).

translate_points(_,_,[],[]).

translate_points(X, Y, [[X1,Y1] | Rest], [[X2,Y2] | TransRest]) :-
  X2 is X1 + X,
  Y2 is Y1 + Y,
  translate_points(X,Y,Rest,TransRest).

  
% The wumpus bitmaps are derived from a closed parabola (y = - x^2 / 3)
%   with two pixels for eyes and a line for a mouth:
%
%             *
%           *   *
%         *       *
%       *           *
%       *           *
%       *   *   *   *
%     *               *
%     *               *
%     *     * * *     *
%   *                   *
%   *                   *
%   * * * * * * * * * * *

wumpus_bitmap_extent(0,11,12).
wumpus_bitmap_extent(90,12,11).
wumpus_bitmap_extent(180,11,12).
wumpus_bitmap_extent(270,12,11).

wumpus_bitmap(0,[
  [1,1],[1,2],[1,3],
  [2,1],[2,4],[2,5],[2,6],
  [3,1],[3,7],[3,8],[3,9],
  [4,1],[4,10],
  [5,1],[5,4],[5,7],[5,11],
  [6,1],[6,4],[6,12],
  [7,1],[7,4],[7,7],[7,11],
  [8,1],[8,10],
  [9,1],[9,7],[9,8],[9,9],
  [10,1],[10,4],[10,5],[10,6],
  [11,1],[11,2],[11,3]
  ]).

wumpus_bitmap(90,[
  [1,6],
  [2,5],[2,7],
  [3,4],[3,8],
  [4,3],[4,9],
  [5,3],[5,9],
  [6,3],[6,5],[6,7],[6,9],
  [7,2],[7,10],
  [8,2],[8,10],
  [9,2],[9,5],[9,6],[9,7],[9,10],
  [10,1],[10,11],
  [11,1],[11,11],
  [12,1],[12,2],[12,3],[12,4],[12,5],[12,6],[12,7],[12,8],[12,9],[12,10],[12,11]
  ]).

wumpus_bitmap(180,[
  [1,10],[1,11],[1,12],
  [2,7],[2,8],[2,9],[2,12],
  [3,4],[3,5],[3,6],[3,12],
  [4,3],[4,12],
  [5,2],[5,6],[5,9],[5,12],
  [6,1],[6,9],[6,12],
  [7,2],[7,6],[7,9],[7,12],
  [8,3],[8,12],
  [9,4],[9,5],[9,6],[9,12],
  [10,7],[10,8],[10,9],[10,12],
  [11,10],[11,11],[11,12]
  ]).

wumpus_bitmap(270,[
  [1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[1,8],[1,9],[1,10],[1,11],
  [2,1],[2,11],
  [3,1],[3,11],
  [4,2],[4,5],[4,6],[4,7],[4,10],
  [5,2],[5,10],
  [6,2],[6,10],
  [7,3],[7,5],[7,7],[7,9],
  [8,3],[8,9],
  [9,3],[9,9],
  [10,4],[10,8],
  [11,5],[11,7],
  [12,6]
  ]).


% The pit bitmaps are derived from an ellipse ((x^2 / 36) + (y^2 / 9) = 1).
%
%              * * * * *
%          * *           * *
%        *                   *
%      *                       *
%        *                   *
%          * *           * *
%              * * * * *

pit_bitmap_extent(0,13,7).
pit_bitmap_extent(90,7,13).
pit_bitmap_extent(180,13,7).
pit_bitmap_extent(270,7,13).

pit_bitmap(0,[
  [1,4],
  [2,3],[2,5],
  [3,2],[3,6],
  [4,2],[4,6],
  [5,1],[5,7],
  [6,1],[6,7],
  [7,1],[7,7],
  [8,1],[8,7],
  [9,1],[9,7],
  [10,2],[10,6],
  [11,2],[11,6],
  [12,3],[12,5],
  [13,4]
  ]).

pit_bitmap(90,[
  [1,5],[1,6],[1,7],[1,8],[1,9],
  [2,3],[2,4],[2,10],[2,11],
  [3,2],[3,12],
  [4,1],[4,13],
  [5,2],[5,12],
  [6,3],[6,4],[6,10],[6,11],
  [7,5],[7,6],[7,7],[7,8],[7,9]
  ]).

pit_bitmap(180,L) :-
  pit_bitmap(0,L).

pit_bitmap(270,L) :-
  pit_bitmap(90,L).


% The gold bitmaps are derived from a trapezoid, whose sides are at 45
%   degree angles to the bottom:
%
%           * * * * * *
%         *             *
%       *                 *
%     *                     *
%   * * * * * * * * * * * * * *

gold_bitmap_extent(0,14,5).
gold_bitmap_extent(90,5,14).
gold_bitmap_extent(180,14,5).
gold_bitmap_extent(270,5,14).

gold_bitmap(0,[
  [1,1],
  [2,1],[2,2],
  [3,1],[3,3],
  [4,1],[4,4],
  [5,1],[5,5],
  [6,1],[6,5],
  [7,1],[7,5],
  [8,1],[8,5],
  [9,1],[9,5],
  [10,1],[10,5],
  [11,1],[11,4],
  [12,1],[12,3],
  [13,1],[13,2],
  [14,1]
  ]).

gold_bitmap(90,[
  [1,5],[1,6],[1,7],[1,8],[1,9],[1,10],
  [2,4],[2,11],
  [3,3],[3,12],
  [4,2],[4,13],
  [5,1],[5,2],[5,3],[5,4],[5,5],[5,6],[5,7],[5,8],[5,9],[5,10],[5,11],[5,12],[5,13],[5,14]
  ]).

gold_bitmap(180,[
  [1,5],
  [2,4],[2,5],
  [3,3],[3,5],
  [4,2],[4,5],
  [5,1],[5,5],
  [6,1],[6,5],
  [7,1],[7,5],
  [8,1],[8,5],
  [9,1],[9,5],
  [10,1],[10,5],
  [11,2],[11,5],
  [12,3],[12,5],
  [13,4],[13,5],
  [14,5]
  ]).

gold_bitmap(270,[
  [1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[1,7],[1,8],[1,9],[1,10],[1,11],[1,12],[1,13],[1,14],
  [2,2],[2,13],
  [3,3],[3,12],
  [4,4],[4,11],
  [5,5],[5,6],[5,7],[5,8],[5,9],[5,10]
  ]).
