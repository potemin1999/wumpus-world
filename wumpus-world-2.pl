index([E|_], E, 1).

index([_|T], E, I) :- 
    index(T, E, I1),  I is I1+1.

/** Movements */
dir_rotate(right,right,down).
dir_rotate(right,left,up).
dir_rotate(down,right,left).
dir_rotate(down,left,right).
dir_rotate(left,right,up).
dir_rotate(left,left,down).
dir_rotate(up,right,right).
dir_rotate(up,left,left).

start_position(1/1).
start_direction(up).

do_move_newpos(Pos,climb,Pos).
do_move_newpos(Pos,grab,Pos).
do_move_newpos(Pos,shoot,Pos).

do_move_newpos(X/Y-right,forward,X1/Y-right) :-
    X1 is X + 1.

do_move_newpos(X/Y-down,forward,X/Y1-down) :-
    Y1 is Y - 1.

do_move_newpos(X/Y-left,forward,X1/Y-left) :-
    X1 is X - 1.

do_move_newpos(X/Y-up,forward,X/Y1-up) :-
    Y1 is Y + 1.

do_move_newpos(X/Y-Dir,Rotation,X/Y-NDir) :-
    ( Rotation = right ; Rotation = left),
    dir_rotate(Dir,Rotation,NDir).

/** World related functions */

get_world_height(4) .
get_world_width(4).

get_world(W) :- 
    W = [1/3-wumpus,2/3-gold,3/3-pit,3/1-pit,4/4-pit].

adjacent(X1/Y1,X2/Y2) :-
    ( X2 is X1 - 1,Y2 = Y1 ) ;
    ( X2 is X1 + 1,Y2 = Y1 ) ;
    ( X2 = X1,Y2 is Y1 - 1 ) ;
    ( X2 = X1,Y2 is Y1 + 1 ) .

visited(Position,History) :-
    HistoryEntry = he(Position-_Direction,_Plan,_Perception,_Action),
    member(HistoryEntry,History).

perception(Position,History,Perception) :-
    HistoryEntry = he(Position-_Direction,_Plan,Perception,_Action),
    member(HistoryEntry,History).

no_wumpus_nearby(Position,History) :-
    adjacent(Position,Neighbour),
    visited(Neighbour,History),
    Perception = [_Breeze,_Bump,_Glitter,_Scream,false],
    perception(Neighbour,History,Perception).

no_pit(Position,History) :-
    adjacent(Position,Neighbour),
    visited(Neighbour,History),
    Perception = [false,_Bump,_Glitter,_Scream,_Stench],
    perception(Neighbour,History,Perception).

in_previous_plan(Position,History) :-
    HistoryEntry = he(_Pos,Plan,_Perc,_Action),
    member(HistoryEntry,History),
    memberchk(Position,Plan).

is_bumping(Pos,forward) :-
    (Pos = 1/_-left  ;
     Pos = _/1-down  ;
     Pos = 4/_-right ;
     Pos = _/4-up    ).

cell_contains(Position,Entity) :-
    get_world(World),
    member(Position-Entity,World).

cell_near(Position,Entity,Percept) :-
    (   cell_contains(Position,Entity)
    ->  Percept = true
    ;   (adjacent(Position,AdjPosition),cell_contains(AdjPosition,Entity))
        ->  Percept = true
        ;   Percept = false
    ).

world_perception(Actions,Position-_Direction,Perception) :-
    cell_near(Position,wumpus,Stench),
    cell_near(Position,pit,Breeze),
    (   (cell_contains(Position,gold),\+ member(ae(Position-_,grab),Actions))
    ->  Glitter = true 
    ;   Glitter = false
    ),
    Perception = [Breeze,_Bump,Glitter,_Scream,Stench].

do_setup(State) :- 
    start_position(Position),
    start_direction(Direction),
    agent_do_setup(AgentSetup),
    State = ag(AgentSetup):ps(Position-Direction):bs(false,false):ac([]).

is_done(State) :-
    State = ag(_Agent):ps(_Position-_Direction):bs(_Bumped,_Screamed):ac(Actions),
    ActionEntry = ae(_Position1,climb),
    member(ActionEntry,Actions).

do_report_and_exit(Reason) :-
    writeln(Reason),
    fail.

/** Validate agent actions */
is_valid_action(Action,_Position-_Direction,_History) :-
    memberchk(Action,[right,left,forward]),!.

is_valid_action(shoot,_Position-_Direction,History) :-
    (   \+ member(ae(_,shoot),History) -> true ; 
        do_report_and_exit('Unable to shoot twice')).

is_valid_action(grab,Position,History) :-
    Perception = [_Breeze,_Bump,true,_Scream,_Stench],
    (   world_perception(History,Position,Perception) -> true ;
        do_report_and_exit('Unable to grab here')).

is_valid_action(climb,Position-_Direction,_History) :-
    (   (start_position(Position)) -> true; 
        do_report_and_exit('Unable to climb here')).

is_no_wumpus(Position-_Direction,History) :-
    (   (\+ cell_contains(Position,wumpus)) -> true ;
        member(ae(Somepos,shoot),History),
        agent_faced_to(Somepos,Position)).

is_valid_position(Position-Direction,History) :-
    (   (\+ cell_contains(Position,pit)) -> true ;
        do_report_and_exit('Dead due to pit')),
    (   is_no_wumpus(Position-Direction,History) -> true ;
        do_report_and_exit('Dead due to wumpus')).

%we can save steps here,and use them after
do_next(State,Action) :-
    Action0 = ae(_Pos,Action),
    State = ag(_):ps(_):bs(_,_):ac([Action0|_]).

do_next(State,done) :-
    is_done(State).

do_next(State0,Status) :-
    \+ is_done(State0),
    State0 = ag(Agent0):ps(Position0):bs(Bumped0,Screamed0):ac(Actions0),
    Perception = [_Breeze,Bumped0,_Glitter,Screamed0,_Stench],
    world_perception(Actions0,Position0,Perception),
    agent_do_next(Agent0,Perception,Action,Agent),
    write('\e[33;1m agent at: '),write(Position0), writeln('\e[0m'),
    ( var(Action) -> do_report_and_exit('Agent does not react.') ; true),
    is_valid_action(Action,Position0,Actions0),
    (   (Action == shoot,cell_contains(WuPos,wumpus),agent_faced_to(Position0,WuPos))
        ->  Scream = true 
        ;   Scream = false
    ),
    (   is_bumping(Position0,Action) 
        ->  Bumped = true,Position = Position0,writeln("bumping")  % no change of position
        ;   Bumped = false, do_move_newpos(Position0,Action,Position),
            Agent = s(Position,_,History),
            NewAgent = s(Position,[],History)
    ),
    is_valid_position(Position,Actions0),
    NewActionEntry = ae(Position,Action),
    NewActions = [NewActionEntry|Actions0],
    NewState = ag(NewAgent):ps(Position):bs(Bumped,Scream):ac(NewActions),
    do_next(NewState,Status).

/**** AGENTS ****/

agent_faced_to(X/Y-right,X1/Y) :- X1 > X.
agent_faced_to(X/Y-down, X/Y1) :- Y1 < Y.
agent_faced_to(X/Y-left, X1/Y) :- X1 < X.
agent_faced_to(X/Y-up,   X/Y1) :- Y1 > Y.

agent_do_setup(State) :-
    State = s(1/1-up,[],[]).

/** agent actions logic */

%agent_do_next(+State0,+Perception,-Action,-State) :-
agent_do_next(State0,Perception,grab,State) :- %action - grab
writeln("do grab"),
    Perception = [_Breeze,_Bump,true,_Scream,_Stench],!,% Gold? ==> grab
    State0 = s(Position,CellHistory,History),
    HistEntry = he(Position,CellHistory,Perception,grab),
    State = s(Position,CellHistory,[HistEntry|History]).

agent_do_next(State0,_Perc,climb,State) :- %action - climb
    State0 = s(Position,[climb],History),
    State = s(Position,[],History),!.

agent_do_next(State0,Perception,Action,State) :- 
    writeln("do forward"),
    Perception = [_Breeze,false,false,_Scream,_Stench],
    State0 = s(Position,CellHistory,History),
    HistEntry = he(Position,[],Perception,forward),
    Action = forward,
    do_move_newpos(Position,Action,NewPosition),
    State = s(NewPosition,CellHistory,[HistEntry|History]).

agent_do_next(State0,Perception,Action,State) :- %action - any,but we have bumped
writeln("do bump"),
    Perception = [_Breeze,true,_Glitter,_Scream,_Stench],!,
    State0 = s(_Position,CellHistory,History),
    History = [HistEntry|_],
    HistEntry = he(Position,_,_,_),
    (Action = left ; Action = right),
    Position = _-Dir,
    \+ member(Dir,CellHistory),
    do_move_newpos(Position,Action,NewPosition),
    NewPosition = _-NewDir,
    NewCellHistory = [NewDir|CellHistory],
    do_move_newpos(Position,Action,NewPosition),
    State = s(NewPosition,NewCellHistory,History).

agent_do_next(State0,Perception,Action,State) :- 
    writeln("do turn"),
    Perception = [_Breeze,false,false,_Scream,_Stench],
    State0 = s(Position,CellHistory,History),
    (Action = left ; Action = right),
    Position = _-Dir,
    \+ member(Dir,CellHistory),
    do_move_newpos(Position,Action,NewPosition),
    NewPosition = _-NewDir,
    NewCellHistory = [NewDir|CellHistory],
    HistEntry = he(Position,[],Perception,Action),
    State = s(NewPosition,NewCellHistory,[HistEntry|History]).

% normally,we only follow the plan we made previously
/*agent_do_next(State0,Perception,Action,State) :- 
    State0 = s(Position,[Plan1|Plans],History),!,
    pos_nextpos_goodmove(Position,Plan1,Action),
    do_move_newpos(Position,Action,Position1-Direction1),
    ( (Position1 = Plan1)
    ->  Nextplan = Plans  % reached desired position,head for next
    ;   Nextplan = [Plan1|Plans]
    ),
    HistEntry = he(Position,[Plan1|Plans],Perception,Action),
    State = s(Position1-Direction1,Nextplan,[HistEntry|History]).

agent_do_next(State0,Perception,Action,State) :-
    State0 = s(PosDir0,[],History0),
    Perception = [_Breeze,false,false,_Scream,_Stench],
    HistoryEntry = he(PosDir0,[],Perception,Action),
    History = [HistoryEntry|History0],
    PosDir0 = Position0-_Direction0,
    ( get_next_target(History,Target) 
    ->  once(get_target_history_plan(Position0,Target,History,Plan)),
        Plan = [Plan1|_],
        pos_nextpos_goodmove(PosDir0,Plan1,Action),
        do_move_newpos(PosDir0,Action,Position),
        State = s(Position,Plan,History)
    ;   once(get_target_history_plan(Position0,1/1,History,RescuePlan0)),
        append(RescuePlan0,[climb],RescuePlan),
        Action = right, % do anything
        do_move_newpos(PosDir0,Action,Position),
        State = s(Position,RescuePlan,History)
    ).*/

/** targeting */
%TODO: remove them
pos_nextpos_goodmove(Position-Direction,Next,Move) :-
    (   agent_faced_to(Position-Direction,Next) 
    ->  Move = forward 
    ;   Move = right 
    ).

get_next_target(History,Target) :-
    visited(Somepos,History),
    adjacent(Somepos,Neighbour),
    \+ visited(Neighbour,History),
    \+ in_previous_plan(Neighbour,History),
    no_wumpus_nearby(Neighbour,History),
    no_pit(Neighbour,History),
    Target = Neighbour.

get_target_history_plan(Pos,Target,_Hist,[Target]) :-
    adjacent(Pos,Target).

get_target_history_plan(Pos,Target,Hist,[Next|Rest]) :-
    adjacent(Pos,Next),
    visited(Next,Hist),
    get_target_history_plan(Next,Target,Hist,Rest).

/** pathfinding */

find_action(Actions) :-
   do_setup(State),
   do_next(State,Actions).

find_actions(Actions2) :-
    findall(Actions,find_action(Actions),Actions2),
    writeln(Actions2).
    