/** Global vars */

%score(Value)
:- dynamic score/1.

mod_score_by(V) :-
    score(S),
    NS is S+V,
    retractall(score(_)),
    assert(score(NS)).

dump_score :-
    score(Score),
    write("Score: "), writeln(Score).

%world(Width,Height,World)
:- dynamic world/3.

set_world_map(World) :-
    world(Width,Height,_),
    retractall(world(_,_,_)),
    assert(world(Width,Height,World)).

get_world_map(World) :- 
    world(_,_,World).

set_world_size(Width,Height) :-
    world(_,_,World),
    retractall(world(_,_,_)),
    assert(world(Width,Height,World)).

get_world_size(Width,Height) :-
    world(Width,Height,_),
    true.

dump_world :-
    world(Width, Height, Map),
    write("World: "), write(Width), write("x"), writeln(Height),
    write("World map: "), writeln(Map).

/**
 * Heading:
 *   1:0  - right
 *   0:1  - up
 *  -1:0  - left
 *   0:-1 - down
 * 
 * IsDead: true/false
 */
%player(CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead)
:- dynamic player/6.

heading_to_string(0:1, up).
heading_to_string(1:0, right).
heading_to_string(0:-1, down).
heading_to_string(-1:0, left).

reset_player(CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead) :-
    retractall(player(_,_,_,_,_,_)),
    assert(player(CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead)).

set_player_coords(X, Y) :-
    player(_,_,Heading,ArrowCount,GoldCount,IsDead),
    reset_player(X,Y,Heading,ArrowCount,GoldCount,IsDead).

get_player_coords(X, Y) :-
    player(X,Y,_,_,_,_).

set_player_heading(Heading) :-
    player(X,Y,_,ArrowCount,GoldCount,IsDead),
    reset_player(X,Y,Heading,ArrowCount,GoldCount,IsDead).

get_player_heading(Heading) :-
    player(_,_,Heading,_,_,_).

set_player_arrow_count(Count) :-
    player(X,Y,Heading,_,GoldCount,IsDead),
    reset_player(X,Y,Heading,Count,GoldCount,IsDead).

get_player_arrow_count(Count) :-
    player(_,_,_,Count,_,_).

set_player_gold_count(Count) :-
    player(X,Y,Heading,ArrowCount,_,IsDead),
    reset_player(X,Y,Heading,ArrowCount,Count,IsDead).

get_player_gold_count(Count) :-
    player(_,_,_,_,Count,_).

set_player_is_dead(IsDead) :-
    player(X,Y,Heading,ArrowCount,GoldCount,_),
    reset_player(X,Y,Heading,ArrowCount,GoldCount,IsDead).

get_player_is_dead(IsDead) :-
    player(_,_,_,_,_,IsDead).

dump_player :-
    player(X, Y, Heading, ArrowCount, GoldCount, IsDead),
    heading_to_string(Heading, HeadingString),
    write("Player at "), write(X), write(":"), write(Y),
    write(", heading "), write(HeadingString), write(", dead:"), write(IsDead),
    write(", arrows: "),write(ArrowCount), write(", golds: "), writeln(GoldCount).

/**
 * State = [Breeze,Bump,Glitter,Scream,Stench]
 * State = [true,  true,false,  false, true  ]
 */
:- dynamic player_state/5.

set_player_state(Breeze,Bump,Glitter,Scream,Stench) :-
    retractall(player_state),
    assert(player_state(Breeze,Bump,Glitter,Scream,Stench)).

get_player_state(Breeze,Bump,Glitter,Scream,Stench) :-
    player_state(Breeze,Bump,Glitter,Scream,Stench).

dump_player_state :-
    player_state(Breeze, Bump, Glitter, Scream, Stench ),
    write("Player state = ["),
    write("breeze:"), write(Breeze), write(", "),
    write("bump:"), write(Bump), write(", "),
    write("glitter:"), write(Glitter), write(", "),
    write("scream:"), write(Scream), write(", "),
    write("stench:"), write(Stench), writeln("]").


setup_variables :- 
    retractall(player_state(_,_,_,_,_)),
    assert(player_state(false,false,false,false,false)),
    retractall(player(_,_,_,_,_,_)),
    assert(player(1,1,0:1,1,0,false)),
    retractall(world(_,_,_)),
    assert(world(0,0,[])),
    retractall(score(_)),
    assert(score(0)).

/** List operations*/

%index(+List, +Element, -Index)

index([E|_], E, 1).

index([_|T], E, I) :- 
    index(T, E, I1),  I is I1+1.

%replace(+List, +Index, +Value, -List)

replace([_|T],  1,  X,  [X|T]).

replace([H|T],  I,  X,  [H|R]) :-
    I > -1, I1 is I-1,
    replace(T,  I1,  X,  R),  !.

replace(L,  _,  _,  L).

/*
 * World operations
 *
 * Possible world array values:
 * gold
 * player
 * pit
 * wumpus
 * wumpus_killed
 */

%world_cell(+World, +X, +Y, -Value)

world_cell([X:Y-Value|_], X, Y, Value).

world_cell([_|World], X, Y, Value) :-
    world_cell(World, X, Y, Value).

%adjacent(+Width,+Height,+X:Y,-X:Y)

adjacent(W,H,X1:Y1,X2:Y2) :-
    ( X2 is X1 - 1, Y2 = Y1 , X1 > 1 ) ;
    ( X2 is X1 + 1, Y2 = Y1 , X1 < W ) ;
    ( X2 = X1, Y2 is Y1 - 1 , Y1 > 0 ) ;
    ( X2 = X1, Y2 is Y1 + 1 , Y1 < H ) .

adjacent(X1:Y1,X2:Y2) :- 
    get_world_size(W,H),
    adjacent(W,H,X1:Y1,X2:Y2).

/** turns */
turn_cw( 1:0 , 0:-1).
turn_cw( 0:-1,-1:0 ).
turn_cw( -1:0, 0:1 ).
turn_cw( 0:1 , 1:0 ).

turn_ccw(A, B) :-
    turn_cw(B, A).

%cell_contains_something(+World, +X, +Y)

cell_contains_gold(World, X, Y) :- 
    world_cell(World, X, Y, gold).

cell_contains_pit(World, X, Y) :- 
    world_cell(World, X, Y, pit).

cell_contains_wumpus(World, X, Y) :- 
    world_cell(World, X, Y, wumpus).

cell_contains_player(World, X, Y) :- 
    world_cell(World, X, Y, player).

setup_world(World, Width, Height) :-
    assert(world(0,0,[])),
    World = [1:1-player, 3:Height-pit, Width:2-pit, 1:Height-gold, Width:Height-wumpus],
    set_world_map(World),
    set_world_size(10,10).

setup_world :-
    setup_world(_, 10, 10).

report_not_found_on_map(Object) :-
    write(Object), writeln(" was not found on map"),
    false.

validate_world(World) :-
    \+ index(World, _:_-player, _) -> report_not_found_on_map(player);
    \+ index(World, _:_-wumpus, _) -> report_not_found_on_map(wumpus);
    \+ index(World, _:_-gold, _)   -> report_not_found_on_map(gold);
    true.

/** Perception */

perceive_breeze(X, Y) :-
    get_world_map(World),
    adjacent(X:Y, Xa:Ya),
    cell_contains_pit(World, Xa, Ya).

perceive_bump(X, Y) :-
    get_world_size(W, H),
    (X > W; Y > H).

perceive_bump(X, Y) :-
    (X < 1; Y < 1).

perceive_glitter(X, Y) :-
    get_world_map(World),
    cell_contains_gold(World, X, Y).

perceive_scream(X, Y) :-
    get_world_map(World),
    world_cell(World, X, Y, wumpus_killed).

perceive_stench(X, Y) :-
    get_world_map(World),
    cell_contains_wumpus(World, X, Y).

perceive_stench(X ,Y) :-
    get_world_map(World),
    adjacent(X:Y, Xa:Ya),
    cell_contains_wumpus(World, Xa, Ya).

%perceive(+X, +Y, -State)
perceive(X, Y, State) :-
    ((perceive_breeze(X,Y) -> Breeze = true ; Breeze = false), true),
    ((perceive_bump(X,Y) -> Bump = true ; Bump = false), true),
    ((perceive_glitter(X,Y) -> Glitter = true ; Glitter = false), true),
    ((perceive_scream(X,Y) -> Scream = true ; Scream = false), true),
    ((perceive_stench(X,Y) -> Stench = true ; Stench = false), true),
    State = [Breeze,Bump,Glitter,Scream,Stench],
    true.



/** Player operations */

%move_player_at(+World, +NewX, +NewY, -World)

move_player_at(X, Y) :-
    get_world_map(World),
    set_player_coords(X, Y),
    cell_contains_player(World, X0, Y0),
    index(World, X0:Y0-player, Index),
    replace(World, Index, X:Y-player, NewWorld),
    set_world_map(NewWorld).

%can_move_forward(-NewCoordX,-NewCoordY)
can_move_forward(NewCoordX,NewCoordY) :-
    get_player_coords(CoordX,CoordY),
    get_player_heading(Hx:Hy),
    NewCoordX is CoordX + Hx,
    NewCoordY is CoordY + Hy,
    \+ perceive_bump(NewCoordX,NewCoordY).

/** Player actions */
%action_forward
action_forward :-
    \+ can_move_forward(_,_),
    get_player_state(S1,_   ,S3,S4,S5),
    set_player_state(S1,true,S3,S4,S5),
    mod_score_by(1).

action_forward :-
    can_move_forward(NewCoordX,NewCoordY),
    move_player_at(NewCoordX,NewCoordY),
    mod_score_by(1).

%action_turn
action_turn_left :-
    get_player_heading(Xh:Yh),
    turn_ccw(Xh:Yh,NXh:NYh),
    set_player_heading(NXh:NYh),
    mod_score_by(1).

action_turn_right :-
    get_player_heading(Xh:Yh),
    turn_cw(Xh:Yh,NXh:NYh),
    set_player_heading(NXh:NYh),
    mod_score_by(1).

%action_grab
action_grab :-
    get_world_map(World),
    get_player_coords(CoordX,CoordY),
    (cell_contains_gold(World,CoordX,CoordY)
     -> set_player_gold_count(1)),
    mod_score_by(1).

%action_shoot
action_shoot :-
    %TODO: arrow magic
    mod_score_by(1).

%action_climb
action_climb :-
    get_player_coords(X,Y),
    X == 1, Y == 1,
    mod_score_by(1000).

/** Pathfinding */

%find_path(World) :-

setup :-
    setup_variables,
    setup_world,
    get_world_map(World),
    validate_world(World). 

run :-
    writeln("Setting up wumpas world"), 
    setup,
    writeln("setup done"), 
    get_world_map(World),
    writeln(World), 
    move_player_at(2, 2),
    cell_contains_player(World,Xp,Yp),
    cell_contains_wumpus(World,Xw,Yw),
    writeln(Xp), writeln(Yp),
    writeln(Xw), writeln(Yw).

dump_game_status :-
    write("\e[33;1m"),
    dump_world,
    dump_score,
    dump_player,
    dump_player_state,
    write("\e[0m").