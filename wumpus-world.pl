/** Global vars */

%score(Value)
:- dynamic score/1.

mod_score_by(V) :-
    score(S),
    NS is S+V,
    retractall(score(_)),
    assert(score(NS)).

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


setup_variables :- 
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

%cell_contains_something(+World, +X, +Y)

cell_contains_gold(World, X, Y) :- 
    world_cell(World, X, Y, gold).

cell_contains_pit(World, X, Y) :- 
    world_cell(World, X, Y, pit).

cell_contains_wumpus(World, X, Y) :- 
    write("Check "),
    write(X),
    write(":"),
    writeln(Y),
    world_cell(World, X, Y, wumpus).

cell_contains_player(World, X, Y) :- 
    world_cell(World, X, Y, player).

setup_world(World, Width, Height) :-
    assert(world(0,0,[])),
    World = [1:1-player, 3:Height-pit, Width:2-pit, 1:Height-gold, Width:Height-wumpus],
    set_world_map(World),
    set_world_size(10,10).

report_not_found_on_map(Object) :-
    write(Object), writeln(" was not found on map"),
    false.

validate_world(World) :-
    \+ index(World, _:_-player, _) -> report_not_found_on_map(player);
    \+ index(World, _:_-wumpus, _) -> report_not_found_on_map(wumpus);
    \+ index(World, _:_-gold, _)   -> report_not_found_on_map(gold);
    true.

/** Perception */

perceive_stench(World, X, Y) :-
    cell_contains_wumpus(World, X, Y) ;
    adjacent(X:Y, Xa:Ya),
    cell_contains_wumpus(World, Xa, Ya).

/** Player operations */

%move_player_at(+World, +NewX, +NewY, -World)

move_player_at(World, X, Y, NewWorld) :-
    cell_contains_player(World, X0, Y0),
    index(World, X0:Y0-player, Index),
    replace(World, Index, X:Y-player, NewWorld).

/** Pathfinding */

%find_path(World) :-

setup :-
    setup_variables,
    setup_world,

run :-
    writeln("Setting up wumpas world"), 
    do_world_setup(World, 10, 10), 
    validate_world(World),
    writeln("setup done"), 
    writeln(World), 
    move_player_at(World, 2, 2, World1),
    cell_contains_player(World,Xp,Yp),
    cell_contains_wumpus(World,Xw,Yw),
    writeln(Xw), writeln(Yw).