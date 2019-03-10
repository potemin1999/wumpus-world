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

dump_world([Width,Height,Map]) :-
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

dump_player([X, Y, Heading, ArrowCount, GoldCount, IsDead]) :-
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
world_cell([], _,_,_) :- false.

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
turn_cw( right , down).
turn_cw( down, left).
turn_cw( left, up).
turn_cw( up, right).

turn_ccw(A, B) :-
    turn_cw(B, A).

dir_to_str(1,0, right).
dir_to_str(0,1, up).
dir_to_str(-1,0, left).
dir_to_str(0,-1, down).

%cell_contains_something(+World, +X, +Y)

cell_contains_gold(WorldMap, X, Y) :- 
    world_cell(WorldMap, X, Y, gold).

cell_contains_pit(WorldMap, X, Y) :- 
    world_cell(WorldMap, X, Y, pit).

cell_contains_wumpus(WorldMap, X, Y) :- 
    world_cell(WorldMap, X, Y, wumpus).

cell_contains_player(WorldMap, X, Y) :- 
    world_cell(WorldMap, X, Y, player).

setup_world(World, Width, Height) :-
    assert(world(0,0,[])),
    WorldMap = [1:1-player, 3:Height-pit, Width:2-pit, 2:Height-gold, 1:2-wumpus],
    set_world_map(WorldMap),
    set_world_size(Width,Height),
    World = [Width,Height,WorldMap].

setup_world :-
    setup_world(_, 5 ,5).

report_not_found_on_map(Object) :-
    write(Object), writeln(" was not found on map"),
    false.

validate_world(World) :-
    \+ index(World, _:_-player, _) -> report_not_found_on_map(player);
    \+ index(World, _:_-wumpus, _) -> report_not_found_on_map(wumpus);
    \+ index(World, _:_-gold, _)   -> report_not_found_on_map(gold);
    true.

/** Perception */

perceive_breeze([_,_,WorldMap], X, Y) :-
    adjacent(X:Y, Xa:Ya),
    cell_contains_pit(WorldMap, Xa, Ya).

perceive_bump([W,H,_], X, Y) :-
    (X > W; Y > H).

perceive_bump([_,_,_], X, Y) :-
    (X < 1; Y < 1).

perceive_glitter([_,_,WorldMap], X, Y) :-
    cell_contains_gold(WorldMap, X, Y).

perceive_scream([_,_,WorldMap], X, Y) :-
    world_cell(WorldMap, X, Y, wumpus_killed).

perceive_stench([_,_,WorldMap], X, Y) :-
    cell_contains_wumpus(WorldMap, X, Y).

perceive_stench([_,_,WorldMap], X ,Y) :-
    adjacent(X:Y, Xa:Ya),
    cell_contains_wumpus(WorldMap, Xa, Ya).

%perceive(+World,+X, +Y, -State)
perceive(World, X, Y, State) :-
    ((perceive_breeze(World, X,Y) -> Breeze = true ; Breeze = false), true),
    ((perceive_bump(World, X,Y) -> Bump = true ; Bump = false), true),
    ((perceive_glitter(World, X,Y) -> Glitter = true ; Glitter = false), true),
    ((perceive_scream(World, X,Y) -> Scream = true ; Scream = false), true),
    ((perceive_stench(World, X,Y) -> Stench = true ; Stench = false), true),
    State = [Breeze,Bump,Glitter,Scream,Stench],
    true.



/** Player operations */

%move_player_at(+Player, +World, +NewX, +NewY, -Player, -World)
move_player_at(Player, World, X, Y, NewPlayer, NewWorld) :-
    Player = [_,_,Heading,ArrowCount,GoldCount,IsDead],
    World = [Width,Height,WorldMap],
    cell_contains_player(WorldMap, X0, Y0),
    index(WorldMap, X0:Y0-player, Index),
    replace(WorldMap, Index, X:Y-player, NewWorldMap),
    NewWorld = [Width,Height,NewWorldMap],
    NewPlayer = [X,Y,Heading,ArrowCount,GoldCount,IsDead].

%can_move_forward(+Player,+World,-NewCoordX,-NewCoordY)
can_move_forward(Player,World,NewCoordX,NewCoordY) :-
    Player = [CoordX,CoordY,Hx:Hy,_,_,_],
    NewCoordX is CoordX + Hx,
    NewCoordY is CoordY + Hy,
    \+ perceive_bump(World,NewCoordX,NewCoordY).

trace_an_arrow(World,Player,NewWorld) :-
    Player = [_,_,Hx:Hy,_,_,_],
    World = [Width,Height,WorldMap],
    index(WorldMap,Xa:Ya-arrow,Index),
    NewX is Xa + Hx, NewY is Ya + Hy,
    \+ perceive_bump(World,NewX,NewY),
    \+ index(WorldMap,NewX:NewY-wall,_),
    \+ index(WorldMap,NewX:NewY-wumpus,_),
    replace(WorldMap, Index, NewX:NewY-arrow, NewWorldMap),
    WorldR = [Width,Height,NewWorldMap],
    trace_an_arrow(WorldR,Player,NewWorld).

trace_an_arrow(World,Player,NewWorld) :-
    Player = [_,_,Hx:Hy,_,_,_],
    World = [Width,Height,WorldMap],
    index(WorldMap,Xa:Ya-arrow,Index),
    NewX is Xa + Hx, NewY is Ya + Hy,
    index(WorldMap,NewX:NewY-wumpus,IndexW),
    replace(WorldMap, IndexW, NewX:NewY-dead_wumpus, WorldMap1),
    replace(WorldMap1, Index, NewX:NewY-shoot_arrow, NewWorldMap),
    NewWorld = [Width,Height,NewWorldMap].

trace_an_arrow(World,Player,NewWorld) :-
    Player = [_,_,Hx:Hy,_,_,_],
    World = [_,_,WorldMap],
    index(WorldMap,Xa:Ya-arrow,_),
    NewX is Xa + Hx, NewY is Ya + Hy,
    perceive_bump(World,NewX,NewY),
    NewWorld = World,
    fail.

/** Player actions */

action_forward(Player,World,NewPlayer,NewWorld) :-
    can_move_forward(Player,World,NewCoordX,NewCoordY),
    move_player_at(Player,World,NewCoordX,NewCoordY,NewPlayer,NewWorld).

%action_turn_left(+Player,+World,-Player,-World)
action_turn_left(Player,World,NewPlayer,NewWorld) :-
    Player = [X,Y,Xh:Yh,ArrowCount,GoldCount,IsDead],
    turn_ccw(Xh:Yh,NXh:NYh),
    NewPlayer = [X,Y,NXh:NYh,ArrowCount,GoldCount,IsDead],
    NewWorld = World.

%action_turn_right(+Player,+World,-Player,-World)
action_turn_right(Player,World,NewPlayer,NewWorld) :-
    Player = [X,Y,Xh:Yh,ArrowCount,GoldCount,IsDead],
    turn_cw(Xh:Yh,NXh:NYh),
    NewPlayer = [X,Y,NXh:NYh,ArrowCount,GoldCount,IsDead],
    NewWorld = World.

%action_grab(+Player,+World,-Player,-World)
action_grab(Player,World,NewPlayer,NewWorld) :-
    Player = [CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead],
    World = [_,_,WorldMap],
    (cell_contains_gold(WorldMap,CoordX,CoordY)
     -> NewGoldCount is GoldCount + 1 ; NewGoldCount = GoldCount),
    NewPlayer = [CoordX,CoordY,Heading,ArrowCount,NewGoldCount,IsDead],
    NewWorld = World.

%action_shoot(+Player,+World,-Player,-World)
action_shoot(Player,World,NewPlayer,NewWorld) :-
    %TODO: arrow magic
    NewPlayer = Player,
    NewWorld = World.

%action_climb(+Player,+World,-Player,-World)
action_climb(Player,_,_,_) :-
    Player = [X,Y,_,_,_,_],
    X == 1, Y == 1.

dump_actions([]).

dump_actions([Action]) :-
    write(Action).

dump_actions([Action|Actions]) :-
    write(Action), write(", "),
    dump_actions(Actions).


/** Pathfinding */

%check_game_state(+World,+Player,+Score,+Actions)
check_game_state(World,Player,_,_,_,Result) :-
    World = [_,_,WorldMap],
    Player = [CoordX,CoordY,_,_,_,_],
    (cell_contains_wumpus(WorldMap,CoordX,CoordY);
     cell_contains_pit(WorldMap,CoordX,CoordY)),
    Result = lose.

check_game_state(World,Player,_,_,_,Result) :-
    World = [_,_,WorldMap],
    Player = [CoordX,CoordY,_,_,_,_],
    \+ cell_contains_wumpus(WorldMap,CoordX,CoordY),
    \+ cell_contains_pit(WorldMap,CoordX,CoordY),
    \+ cell_contains_gold(WorldMap,CoordX,CoordY),
    Result = continue.

check_game_state(World,Player,Score,Actions,ActionsReturn,Result) :-
    World = [_,_,_],
    Player = [CoordX,CoordY,_,_,GoldCount,_],
    CoordX = 1, CoordY = 1,
    GoldCount = 1,
    writeln("\e[37;1m You won! \e[0m"),
    write("\e[32;1m"),
    write("Score: "),writeln(Score),
    dump_player(Player),
    dump_world(World),
    append(Actions, [CoordX:CoordY-climb-1], NewActions),
    dump_actions(NewActions),
    write("\e[0m"),
    ActionsReturn = NewActions,
    Result = won.

%find_path(+World,+Player,+Score,+Actions)
:- dynamic find_path/4.

find_path_by_move(World,Player,Score,Actions,ActionsReturn,DX,DY) :-
    Player = [X,Y,Hx:Hy,_,GoldCount,_],
    not(index(Actions,X:Y-forward-GoldCount,_)),
    NewX is X + DX, NewY is Y + DY,
    \+ perceive_bump(World, NewX, NewY),
    (
        (
            (dir_to_str(Hx,Hy,Hdir), dir_to_str(DX,DY,Ddir), turn_cw(Hdir,D),turn_cw(D,Ddir))
        ) -> (
            append(Actions, [X:Y-turn_right-GoldCount,
                X:Y-turn_right-GoldCount], Actions2),
            NewScore is Score - 3 )
        ;(Hx = DX, Hy = DY) -> (
            Actions2 = Actions,
            NewScore is Score - 1 )
         ;(  
            ( (dir_to_str(Hx,Hy,Hdir), dir_to_str(DX,DY,Ddir), turn_cw(Hdir,Ddir)) ->
                Dir = turn_right ; Dir = turn_left),
            append(Actions, [X:Y-Dir-GoldCount], Actions2),
            NewScore is Score - 2)
    ),
    move_player_at(Player, World, NewX, NewY, Player1, NewWorld),
    Player1 = [X1,Y1,_,ArrowCount1,GoldCount1,IsDead],
    NewPlayer = [X1,Y1,DX:DY,ArrowCount1,GoldCount1,IsDead],
    append(Actions2, [X:Y-forward-GoldCount], NewActions),
    find_path(NewWorld,NewPlayer,NewScore,NewActions,ActionsReturn).

find_path_by_action(World,Player,Score,Actions,ActionsReturn) :-
    find_path_by_move(World,Player,Score,Actions,ActionsReturn,0,1).

find_path_by_action(World,Player,Score,Actions,ActionsReturn) :-
    find_path_by_move(World,Player,Score,Actions,ActionsReturn,0,-1).

find_path_by_action(World,Player,Score,Actions,ActionsReturn) :-
    find_path_by_move(World,Player,Score,Actions,ActionsReturn,1,0).

find_path_by_action(World,Player,Score,Actions,ActionsReturn) :-
    find_path_by_move(World,Player,Score,Actions,ActionsReturn,-1,0).

find_path_by_action(World,Player,Score,Actions,ActionsReturn) :-
    Player = [X,Y,Hx:Hy,ArrowCount,GoldCount,IsDead],
    World = [Width,Height,WorldMap],
    ArrowCount = 1,
    perceive_stench(World,X,Y),
    NewArrowCount is ArrowCount - 1,
    append(WorldMap,[X:Y-arrow],WorldMap1),
    World1 = [Width,Height,WorldMap1],
    trace_an_arrow(World1,Player,NewWorld),
    NewPlayer = [X,Y,Hx:Hy,NewArrowCount,GoldCount,IsDead],
    NewScore is Score - 1,
    append(Actions,[X:Y-shoot],NewActions),
    find_path(NewWorld,NewPlayer,NewScore,NewActions,ActionsReturn).



find_path(World,Player,Score,Actions,ActionsReturn) :-
    check_game_state(World,Player,Score,Actions,ActionsReturn,Result),
    Result = lose,
    fail.

find_path(World,Player,Score,Actions,ActionsReturn) :-
    check_game_state(World,Player,Score,Actions,ActionsReturn,Result),
    Result = won.

find_path(World,Player,Score,Actions,ActionsReturn) :-
    Player = [X,Y,P1,P2,_,P4],
    World = [Width,Height,WorldMap],
    (cell_contains_gold(WorldMap,X,Y) -> (
            index(WorldMap, Xg:Yg-gold, Index),
            replace(WorldMap, Index, Xg:Yg-no_gold, NewWorldMap),
            NewPlayer = [X,Y,P1,P2,1,P4],
            append(Actions,[X:Y-grab],NewActions)
            %writeln("\e[32;1m gold picked up \e[0m")
        ) ; NewPlayer = Player, NewWorldMap = WorldMap, NewActions = Actions),
    NewWorld = [Width,Height,NewWorldMap],
    check_game_state(NewWorld,NewPlayer,Score,NewActions,ActionsReturn,Result),
    Result = continue,
    find_path_by_action(NewWorld,NewPlayer,Score,NewActions,ActionsReturn).

%find_path(World) :-
find_path_on_current_state(Actions) :-
    player(CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead),
    Player = [CoordX,CoordY,Heading,ArrowCount,GoldCount,IsDead],
    world(Width,Height,WorldMap),
    World = [Width,Height,WorldMap],
    find_path(World,Player,0,[],Actions).

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