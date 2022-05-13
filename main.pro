% omer faruk celik

:- ['cmpecraft.pro'].

:- init_from_map.


% abs(A-C) + abs(B-D)
manhattan_distance([A|B], [C|D], E) :- 
    abs_val_subtraction(A,C,O1),
    abs_val_subtraction(B,D,O2),
    E is O1 + O2.

% absolute value subtraction
abs_val_subtraction(A,B,C):- 
    C is A-B, C >= 0, ! ; 
    C is B-A.



minimum_of_list([A], A) :- !.
minimum_of_list([A,B|C],D) :- 
    (A<B -> 
    minimum_of_list([A|C],D) ; 
    A>=B, minimum_of_list([B|C],D)).



 find_nearest_type([A,O,_], ObjType, ObjKey, Object, Distance) :- 
    get_dict(x, A, X),
    get_dict(y, A, Y),
    %creating a set that holds manhattan distances of all objects of type ObjType from the agent
    findall(Dist,(get_dict(_,O,Obj),get_dict(type,Obj,ObjType),manhattan_of_object(X,Y,Obj,Dist)),SetDist),
    %find minimum distance
    minimum_of_list(SetDist,Distance),
    %specify the Object
    get_dict(ObjKey,O,Object),get_dict(type,Object,ObjType),manhattan_of_object(X,Y,Object,Distance),!.

%calculate manhattan distance of specified object with X,Y
manhattan_of_object(X,Y,Obj,Distance):-
    get_dict(x, Obj, TmpX),
    get_dict(y, Obj, TmpY),
    manhattan_distance([X,Y],[TmpX,TmpY],Distance).


navigate_to([A,_,_], X, Y, ActionList, DepthLimit) :- 
    get_dict(x, A, AX),
    get_dict(y, A, AY),
    manhattan_distance([AX,AY],[X,Y],Distance),Distance =< DepthLimit,
    routeBuilder(X,Y,AX,AY,ActionList),!.
    
%Creates an action list that provides transportation between two points
routeBuilder(DestX,DestY,DestX,DestY,[]).
routeBuilder(DestX,DestY,LocX,LocY,[go_right|T]):-  (DestX>LocX),NewLocX is LocX+1,routeBuilder(DestX,DestY,NewLocX,LocY,T).
routeBuilder(DestX,DestY,LocX,LocY,[go_up|T]):-  (DestY<LocY),NewLocY is LocY-1,routeBuilder(DestX,DestY,LocX,NewLocY,T).
routeBuilder(DestX,DestY,LocX,LocY,[go_left|T]):-  (DestX<LocX),NewLocX is LocX-1,routeBuilder(DestX,DestY,NewLocX,LocY,T).
routeBuilder(DestX,DestY,LocX,LocY,[go_down|T]):-  (DestY>LocY),NewLocY is LocY+1,routeBuilder(DestX,DestY,LocX,NewLocY,T).


chop_nearest_tree([A,O,T], ActionList) :- 
    get_dict(x, A, LocX),
    get_dict(y, A, LocY),
    %find nearest tree
    find_nearest_type([A,O,T], tree, _ , Object, _),
    get_dict(x, Object, DestX),
    get_dict(y, Object, DestY),
    %action list to reach and chop the nearest tree
    routeBuilder(DestX,DestY,LocX,LocY,HeadActionList),
    appendlists(HeadActionList,[left_click_c,left_click_c,left_click_c,left_click_c],ActionList),!.

%concatenate two list
appendlists([],R,R).
appendlists([H|T],R,[H|Z]):- appendlists(T,R,Z).


mine_nearest_stone([A,O,T], ActionList) :- 
    get_dict(x, A, LocX),
    get_dict(y, A, LocY),
    %find nearest stone
    find_nearest_type([A,O,T], stone, _ , Object, _),
    get_dict(x, Object, DestX),
    get_dict(y, Object, DestY),
    %action list to reach and mine the nearest stone
    routeBuilder(DestX,DestY,LocX,LocY,HeadActionList),
    appendlists(HeadActionList,[left_click_c,left_click_c,left_click_c,left_click_c],ActionList),!.



gather_nearest_food([A,O,T], ActionList) :- 
    get_dict(x, A, LocX),
    get_dict(y, A, LocY),
    %find nearest food
    find_nearest_type([A,O,T], food, _ , Object, _),
    get_dict(x, Object, DestX),
    get_dict(y, Object, DestY),
    %action list to reach and gather the nearest food
    routeBuilder(DestX,DestY,LocX,LocY,HeadActionList),
    appendlists(HeadActionList,[left_click_c],ActionList),!.


collect_requirements([A,O,T], ItemType, ActionList) :-
    item_req([A,O,T],ItemType,ActionList),!.

    
%collect stick for axes
stick_for_axes([A,O,T],stick,List):-
    get_dict(inventory, A, Inv),
    (get_dict(log, Inv, Count) ->
    %Since stone pickaxe and stone axe already require 3 logs, we continue to process 3 logs less.
    LogCount is Count-3
    ;
    %give 0 if the log field has not initialized
    LogCount is 0),
    ((LogCount < 2) ->  
    chop_nearest_tree([A,O,T],ListTmp),
    appendlists(ListTmp,[craft_stick],List)
    ;
    appendlists([craft_stick],[],List)
    ).
%collect requirements for stick
item_req([A,O,T],stick,FinalList):-
    get_dict(inventory, A, Inv),
    (get_dict(log, Inv, Count) ->
    LogCount is Count
    ;
    %give 0 if the log field has not initialized
    LogCount is 0),
    ((LogCount < 2) ->  
    chop_nearest_tree([A,O,T],FinalList)
    ;
    appendlists([],[],FinalList)
    ).
%collect requirements for stone pickaxe
item_req([A,O,T],stone_pickaxe,FinalList):-
    get_dict(inventory, A, Inv),
    (get_dict(log, Inv, Count) ->
    LogCount is Count
    ;
    %give 0 if the log field has not initialized
    LogCount is 0),
    ((LogCount < 3) -> 
    chop_nearest_tree([A,O,T],List1),
    execute_actions([A,O,T],List1,[A1,O1,T1])
    ;
    [A1,O1,T1] = [A,O,T]
    ),
    get_dict(inventory, A1, Inv1),
    (get_dict(cobblestone, Inv1, Count) ->
    CobCount is Count
    ;
    %give 0 if the cobblestone field has not initialized
    CobCount is 0),
% collect cobblestone with stone
    (((CobCount < 3) ->
    mine_nearest_stone([A1,O1,T1],List2),
    execute_actions([A1,O1,T1],List2,[A2,O2,T2])
    ;
    [A2,O2,T2] = [A1,O1,T1]
    )
    ;
% collect cobblestone with cobblestone
    (
        (CobCount = 0 ,
        mine_nearest_cobblestone([A1,O1,T1],List12),
        execute_actions([A1,O1,T1],List12,[A12,O12,T12]),
        mine_nearest_cobblestone([A12,O12,T12],List13),
        execute_actions([A12,O12,T12],List13,[A13,O13,T13]),
        mine_nearest_cobblestone([A13,O13,T13],List14),
        execute_actions([A13,O13,T13],List14,[A2,O2,T2]),
        appendlists(List12,List13,List1213),
        appendlists(List1213,List14,List2)
        );
        (CobCount = 1 ,
        mine_nearest_cobblestone([A1,O1,T1],List12),
        execute_actions([A1,O1,T1],List12,[A12,O12,T12]),
        mine_nearest_cobblestone([A12,O12,T12],List13),
        execute_actions([A12,O12,T12],List13,[A2,O2,T2]),
        appendlists(List12,List13,List2)
        );
        (CobCount = 2 ,
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2])
        )

    ))
    ,
    get_dict(inventory, A2, Inv2),
    (get_dict(stick, Inv2, Count) ->
    StickCount is Count
    ;
    %give 0 if the stick field has not initialized
    StickCount is 0),
    ((StickCount<2) -> 
        stick_for_axes([A2,O2,T2],stick,List3);
        appendlists([],[],List3)
    ),
    
    appendlists(List1,List2,ListHead),
    appendlists(ListHead,List3,FinalList).

%collect requirements for stone axe  
item_req([A,O,T],stone_axe,FinalList):-
    get_dict(inventory, A, Inv),
    (get_dict(log, Inv, Count) ->
    LogCount is Count
    ;
    %give 0 if the log field has not initialized
    LogCount is 0),
    ((LogCount < 3) -> 
    chop_nearest_tree([A,O,T],List1),
    execute_actions([A,O,T],List1,[A1,O1,T1])
    ;
    [A1,O1,T1] = [A,O,T]
    ),
    get_dict(inventory, A1, Inv1),
    (get_dict(cobblestone, Inv1, Count) ->
    CobCount is Count
    ;
    %give 0 if the cobblestone field has not initialized
    CobCount is 0),
% collect cobblestone with stone
    (((CobCount < 3) ->
    mine_nearest_stone([A1,O1,T1],List2),
    execute_actions([A1,O1,T1],List2,[A2,O2,T2])
    ;
    [A2,O2,T2] = [A1,O1,T1]
    )
    ;
% collect cobblestone with cobblestone
    (
        (CobCount = 0 ,
        mine_nearest_cobblestone([A1,O1,T1],List12),
        execute_actions([A1,O1,T1],List12,[A12,O12,T12]),
        mine_nearest_cobblestone([A12,O12,T12],List13),
        execute_actions([A12,O12,T12],List13,[A13,O13,T13]),
        mine_nearest_cobblestone([A13,O13,T13],List14),
        execute_actions([A13,O13,T13],List14,[A2,O2,T2]),
        appendlists(List12,List13,List1213),
        appendlists(List1213,List14,List2)
        );
        (CobCount = 1 ,
        mine_nearest_cobblestone([A1,O1,T1],List12),
        execute_actions([A1,O1,T1],List12,[A12,O12,T12]),
        mine_nearest_cobblestone([A12,O12,T12],List13),
        execute_actions([A12,O12,T12],List13,[A2,O2,T2]),
        appendlists(List12,List13,List2)
        );
        (CobCount = 2 ,
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2])
        )

    ))
    ,
    get_dict(inventory, A2, Inv2),
    (get_dict(stick, Inv2, Count) ->
    StickCount is Count
    ;
    %give 0 if the stick field has not initialized
    StickCount is 0),
    ((StickCount<2) -> 
        stick_for_axes([A2,O2,T2],stick,List3);
        appendlists([],[],List3)
    ),
    appendlists(List1,List2,ListHead),
    appendlists(ListHead,List3,FinalList)
    .


find_castle_location([_,O,_], XMin, YMin,XMax, YMax) :- 
    %get all objects locations
    findall([X,Y],(get_dict(_,O,Obj),get_dict(x,Obj,X),get_dict(y,Obj,Y)),SetObj),
    width(W),
    C is W -2,
    height(H),
    R is H-2,
    %x and y ranges 1 to W-2  and 1 to H-2
    interval(1,C,XNumRange),
    interval(1,R,YNumRange),
    member(XMin,XNumRange),
    member(YMin,YNumRange),
    YMax is YMin +2,YMax =< R,
    XMax is XMin +2,XMax =<C,
    YInt is YMin +1,YInt =< R,
    XInt is XMin +1,XInt =< C,
    %find 9 free points
    not(member([XMin,YMin],SetObj)),
    not(member([XMin,YInt],SetObj)),
    not(member([XMin,YMax],SetObj)),
    not(member([XInt,YMin],SetObj)),
    not(member([XInt,YInt],SetObj)),
    not(member([XInt,YMax],SetObj)),
    not(member([XMax,YMin],SetObj)),
    not(member([XMax,YInt],SetObj)),
    not(member([XMax,YMax],SetObj)),!.

member(X, [X|_]).     
member(X, [_|Tail]) :-   member(X, Tail). 

interval(LV,LV,[LV]) :- !.
interval(LV,HV,[LV|Ls]) :-
    LV =< HV,
    Z is LV+1,
    interval(Z,HV,Ls).


make_castle([A,O,T], ActionList) :- 
    get_dict(inventory, A, Inv),
    (get_dict(cobblestone, Inv, Count) ->
    CobCount is Count ;
    %give 0 if the cobblestone field has not initialized
    CobCount is 0),
    %collecting cobblestone 
    (
    (CobCount = 0 ,
    ((  
        %3 stone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_stone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListTmp),
        appendlists(ListTmp,List3,ListLast)
    );
    (   
        %2 stone and 3 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(Tmp12,List5,ListLast)
    );
    (
        %1 stone and 6 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[A6,O6,T6]),
        mine_nearest_cobblestone([A6,O6,T6],List7),
        execute_actions([A6,O6,T6],List7,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(Tmp12,Tmp3,Tmp123),
        appendlists(Tmp123,List7,ListLast)
    );
    (
        %9 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[A6,O6,T6]),
        mine_nearest_cobblestone([A6,O6,T6],List7),
        execute_actions([A6,O6,T6],List7,[A7,O7,T7]),
        mine_nearest_cobblestone([A7,O7,T7],List8),
        execute_actions([A7,O7,T7],List8,[A12,O12,T12]),
        mine_nearest_cobblestone([A12,O12,T12],List9),
        execute_actions([A12,O12,T12],List9,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(List7,List8,Tmp4),
        appendlists(Tmp3,Tmp4,Tmp34),
        appendlists(Tmp12,Tmp34,Tmp1234),
        appendlists(Tmp1234,List9,ListLast)
    )
    )
    )
    ;
    (CobCount = 1 ,
    (
        (
        %3 stone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_stone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListTmp),
        appendlists(ListTmp,List3,ListLast)
        )
        ;
        (
        %2 stone and 2 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,ListLast)
        )
        ;
        (
        %1 stone and 5 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(Tmp12,Tmp3,ListLast)
        )
        ;
        (
        %8 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[A6,O6,T6]),
        mine_nearest_cobblestone([A6,O6,T6],List7),
        execute_actions([A6,O6,T6],List7,[A7,O7,T7]),
        mine_nearest_cobblestone([A7,O7,T7],List8),
        execute_actions([A7,O7,T7],List8,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(List7,List8,Tmp4),
        appendlists(Tmp3,Tmp4,Tmp34),
        appendlists(Tmp12,Tmp34,ListLast)
        )
    )
    
     );

    (CobCount = 2 ,
    
    (
        (
        %3 stone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_stone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListTmp),
        appendlists(ListTmp,List3,ListLast)
        )
        ;
        (
        %2 stone and 1 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(Tmp1,List3,ListLast)
        )
        ;
        (
        %1 stone and 4 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(Tmp12,List5,ListLast)
        )
        ;
        (
        %7 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[A6,O6,T6]),
        mine_nearest_cobblestone([A6,O6,T6],List7),
        execute_actions([A6,O6,T6],List7,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(Tmp3,List7,Tmp34),
        appendlists(Tmp12,Tmp34,ListLast)
        
        )
    )
    
    );

    (CobCount = 3 ,
    
    (
        (
        %2 stone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListLast)
        )
       
        ;
        (
        %1 stone and 3 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,ListLast)
        )
        ;
        (
        %6 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[A5,O5,T5]),
        mine_nearest_cobblestone([A5,O5,T5],List6),
        execute_actions([A5,O5,T5],List6,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(List5,List6,Tmp3),
        appendlists(Tmp12,Tmp3,ListLast)
        )
    )
    );
    (CobCount = 4 ,
    (
        (
        %2 stone 
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListLast)
        )
       
        ;
        (
        %1 stone and 2 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(Tmp1,List3,ListLast)
        )
        ;
        (
        %5 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[A4,O4,T4]),
        mine_nearest_cobblestone([A4,O4,T4],List5),
        execute_actions([A4,O4,T4],List5,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,Tmp12),
        appendlists(Tmp12,List5,ListLast)
        
        )
    )
    );
    (CobCount = 5 ,
    (
        (
        %2 stone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_stone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListLast)
        )
       
        ;
        (
        %1 stone and 1 cobblestone
        mine_nearest_stone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListLast)
        )
        ;
        (
        %4 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[A3,O3,T3]),
        mine_nearest_cobblestone([A3,O3,T3],List4),
        execute_actions([A3,O3,T3],List4,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(List3,List4,Tmp2),
        appendlists(Tmp1,Tmp2,ListLast)
        )
    )
    );
    (CobCount = 6 ,
    (
        (
        %1 stone
        mine_nearest_stone([A,O,T],ListLast),
        execute_actions([A,O,T],ListLast,[ALast,OLast,TLast])
        )
        ;
        (
        %3 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[A2,O2,T2]),
        mine_nearest_cobblestone([A2,O2,T2],List3),
        execute_actions([A2,O2,T2],List3,[ALast,OLast,TLast]),
        appendlists(List1,List2,Tmp1),
        appendlists(Tmp1,List3,ListLast)
        )
    )
    );

    (CobCount = 7 ,
    (
        (
        %1 stone
        mine_nearest_stone([A,O,T],ListLast),
        execute_actions([A,O,T],ListLast,[ALast,OLast,TLast])
        )
        ;
        (
        %2 cobblestone
        mine_nearest_cobblestone([A,O,T],List1),
        execute_actions([A,O,T],List1,[A1,O1,T1]),
        mine_nearest_cobblestone([A1,O1,T1],List2),
        execute_actions([A1,O1,T1],List2,[ALast,OLast,TLast]),
        appendlists(List1,List2,ListLast)
        )
    )
    );

    (CobCount = 8 ,
    (
        (
        %1 stone
        mine_nearest_stone([A,O,T],ListLast),
        execute_actions([A,O,T],ListLast,[ALast,OLast,TLast])
        )
        ;
        (
        %1 cobblestone
        mine_nearest_cobblestone([A,O,T],ListLast),
        execute_actions([A,O,T],ListLast,[ALast,OLast,TLast])
        )
    )
    );
    (
    CobCount >= 9,
    appendlists([],[],ListLast),
    [ALast,OLast,TLast] = [A,O,T]
    )
    ),
    %placing cobblestones
    find_castle_location([ALast,OLast,TLast], XMin, YMin,_, _),
    XCenter is XMin +1,
    YCenter is YMin +1,
    get_dict(x, ALast, LocX),
    get_dict(y, ALast, LocY),
    routeBuilder(XCenter,YCenter,LocX,LocY,RouteList),
    appendlists(ListLast,RouteList,FirstList),
    PlacingCobbles = [place_c,place_e,place_w,place_n,place_s,place_nw,place_sw,place_ne,place_se],
    appendlists(FirstList,PlacingCobbles,ActionList),!
    .


mine_nearest_cobblestone([A,O,T],ActionList):-
    get_dict(x, A, LocX),
    get_dict(y, A, LocY),
    find_nearest_type([A,O,T], cobblestone, _ , Object, _),
    get_dict(x, Object, DestX),
    get_dict(y, Object, DestY),
    routeBuilder(DestX,DestY,LocX,LocY,HeadActionList),
    appendlists(HeadActionList,[left_click_c,left_click_c,left_click_c,left_click_c],ActionList),!.