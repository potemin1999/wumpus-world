dir_rotate(right, right, down).
dir_rotate(right, left, up).
dir_rotate(down, right, left).
dir_rotate(down, left, right).
dir_rotate(left, right, up).
dir_rotate(left, left, down).
dir_rotate(up, right, right).
dir_rotate(up, left, left).

start_position(1:1).
start_direction(up).

do_move_newpos(X:Y-right,forward,X1:Y-right) :-
    X1 is X + 1.

do_move_newpos(X:Y-down,forward,X:Y1-down) :-
    Y1 is Y - 1.

do_move_newpos(X:Y-left,forward,X1:Y-left) :-
    X1 is X - 1.

do_move_newpos(X:Y-up,forward,X:Y1-up) :-
    Y1 is Y + 1.

do_move_newpos(X:Y-Dir,Rotation,X:Y-NDir) :-
    ( Rotation=right ; Rotation=left),
    dir_rotate(Dir,Rotation,NDir).

adjacent(X1:Y1,X2:Y2) :-
    ( X2 is X1 - 1, Y2 = Y1 ) ;
    ( X2 is X1 + 1, Y2 = Y1 ) ;
    ( X2 = X1, Y2 is Y1 - 1 ) ;
    ( X2 = X1, Y2 is Y1 + 1 ) .

visited(Position, History) :-
    HistoryEntry = he(Position-_Direction,_Plan,_Perception,_Action),
    member(HistoryEntry,History).

perception(Position,History,Perception) :-
    HistoryEntry = he(Position-_Dir,_Plan,Perception,_Action),
    member(HistoryEntry,History).

no_wumpus_nearby(Position, History) :-
    adjacent(Position,Neighbour),
    visited(Neighbour,History),
    Perception = [_Breeze,_Bump,_Glitter,_Scream, false],
    perception(Neighbour,History,Perception).

no_pit(Position, History) :-
    adjacent(Position, Neighbour),
    visited(Neighbour, History),
    Perception = [true,_Bump,_Glitter,_Scream,_Stench],
    perception(Neighbour,History,Perception).

in_previous_plan(Position, History) :-
    HistoryEntry = he(_Pos,Plan,_Perc,_Action),
    member(HistoryEntry, History),
    memberchk(Position, Plan).

get_next_target(History,Target) :-
    visited(Somepos, History),
    adjacent(Somepos, Neighbour),
    \+ visited(Neighbour, History),
    \+ in_previous_plan(Neighbour, History),
    no_wumpus_nearby(Neighbour, History),
    no_pit(Neighbour, History),
    Target = Neighbour.

get_world(W) :- 
    W = [1:3-wumpus,2:3-gold,3:3-pit,3:1-pit,4:4-pit].

is_bumping(Pos,forward) :-
    (Pos = 1:_-left  ;
     Pos = _:1-down  ;
     Pos = 4:_-right ;
     Pos = _:4-up    ).

cell_contains(Position,Entity) :-
    get_world(World),
    member(Position-Entity,World).

cell_near(Position,Entity,Percept) :-
    (   cell_contains(Position,Entity) ->
        Percept = true
    ;   adjacent(Position,AdjPosition),
        cell_contains(AdjPosition,Entity) ->
        Percept = true
    ;   Percept = false
    ).

world_perception(Actions,Position,_Direction,Perception) :-
    writeln("world perception"),
    cell_near(Position,wumpus,Stench),
    writeln("checked stench"),
    cell_near(Position,pit,Breeze),
    writeln("checked breeze"),
    ((cell_contains(Position,gold), \+ member([Position-_,grab],Actions)) ->
            Glitter = true ; Glitter = false ),
    writeln("checked glitter"),
    Perception = [Breeze,_Bump,Glitter,_Scream,Stench].

do_report_and_exit(Reason) :-
    writeln(Reason),
    fail.

do_setup(State) :- 
    start_position(PosX:PosY),
    start_direction(Direction),
    agent_do_setup(AgentSetup),
    State = ag(AgentSetup):ps(PosX:PosY-Direction):bs(false,false):ac([]).

is_done(State) :-
    State = ag(_Agent):ps(_Position-_Direction):bs(_Bumped,_Screamed):ac(Actions),
    ActionEntry = ae(_Position1,climb),
    member(ActionEntry, Actions).

/** Validate agent actions */
is_valid_action(Action, _Position, _Direction, _Hist) :-
    memberchk(Action, [right,left,forward]),!.

is_valid_action(shoot, _Position, _Direction, History) :-
    (   member([_,shoot], History) ->
        do_report_and_exit('Agent tried to shoot again!')
    ;   true
    ).

is_valid_action(grab, Position, _Direction, History) :-
    Perception = [_Breeze,_Bump,true,_Scream,_Stench],
    (   world_perception(History, Position, Perception) -> true
    ;   do_report_and_exit('Cannot grab here!')).

is_valid_action(climb, Position,_Direction, _History) :-
    (   (start_position(Position)) -> true
    ;   do_report_and_exit('Cannot climb out here.')).


is_no_wumpus(Position, Direction,History) :-
    (   cell_contains(Position, wumpus) ->
        member(pa(Somepos,shoot), History),
        agent_faced_to(Somepos, Position)
    ;   true ).

is_valid_position(Position,Direction,History) :-
    (   cell_contains(Position,Direction, pit) -> do_report_and_exit('Fell into pit - dead.') ; true  ),
    (   is_no_wumpus(Position, Direction,History) -> true ; do_report_and_exit('Ran into wumpus - dead.')).

do_next(State, Action) :-
    Action0 = [_Pos,Action],
    State = ag(_):ps(_):bs(_,_):ac([Action0|_]).

do_next(State, done) :-
    is_done(State).

do_next(State0, S) :-
        \+ is_done(State0),
        writeln("not done"),
        State0 = ag(Agent0):ps(Position0-Direction0):bs(Bumped0,Screamed0):ac(Actions0),
        Perception = [_Breeze,Bumped0,_Glitter,Screamed0,_Stench],
        writeln("perception...."),
        world_perception(Actions0, Position0,Direction0, Perception),
        writeln("percepted"),
        agent_do_next(Agent0, Perception, Action, Agent),
         nl, write('agent at: '), write(Position0),
        %( var(Action) -> c_error('Agent does not react.') ; true),
        is_valid_action(Action, Position0, Actions0),
        ((  Action == shoot,
            cell_contains(WuPos,wumpus),
            agent_faced_to(Position0,WuPos)
         ) -> Scream = true ; Scream = false),
        (   is_bumping(Position0, Action) ->
                Bumped = true, Position = Position0  % no change of position
        ;
                Bumped = false, do_move_newpos(Position0, Action, Position)
        ),
        is_valid_position(Position, Actions0),
        NewActionEntry = ae(Position,Action),
        NewActions = [NewActionEntry|Actions0],
        NewState = ag(Agent):ps(Position):bs(Bumped,Scream):ac(NewActions),
        do_next(NewState, S).

/**** AGENTS ****/

agent_faced_to(X:Y-right, X1:Y) :- X1 > X.
agent_faced_to(X:Y-down,  X:Y1) :- Y1 < Y.
agent_faced_to(X:Y-left,  X1:Y) :- X1 < X.
agent_faced_to(X:Y-up,    X:Y1) :- Y1 > Y.

agent_do_setup(State) :-
    State = s(1:1-up,[],[]).

%agent_do_next(+State0, +Perception, -Action, -State) :-
agent_do_next(State0, Perception, Action, State) :- %action - grab
    Action = grab,
    Perception = [_Breeze,_Bump,true,_Scream,_Stench], !, % Gold? ==> grab
    State0 = s(Position,Plan,History),
    HistEntry = [Position,Plan,Perception,grab],
    State = s(Position,Plan,[HistEntry|History]).

agent_do_next(State0, Perception, Action, State) :- %action - any, but we have bumped
    Perception = [_Breeze,true,_Glitter,_Scream,_Stench],
    State0 = s(_Position,_Plan,History),
    History = [HistEntry,_],
    HistEntry = [Position,_,_,_],
    Action = left,
    do_move_newpos(Position,Action,NewPosition),
    State = s(NewPosition,[],History).

agent_do_next(State0, _Perc, Action, State) :- %action - climb
    Action = climb,
    State0 = s(Pos,[climb],Hist),
    State = s(Pos,[],Hist),
    !.

find_action(Actions) :-
   do_setup(State), 
   do_next(State,Actions).

find_actions(Actions2) :-
    findall(Actions, find_action(Actions), Actions2).
    