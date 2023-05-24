:- dynamic tab/1.

tab(0) :- !.
tab(X) :- X \== 0, write('\x2001'), Y is X-1, tab(Y).

empty_queue([]).
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).
dequeue(E, [E|T], T).

member_queue(Element, Queue) :- member(Element, Queue).
add_list_to_queue(List, Queue, Newqueue) :- append(Queue, List, Newqueue).

empty_stack([]).
stack(Top, Stack, [Top|Stack]).
member_stack(Element, Stack):-member(Element, Stack).
add_list_to_stack(List, Stack, Result):-append(List, Stack, Result).
print_stack(S):-empty_stack(S).
print_stack(S):-stack(E, Rest, S), write(E), write(' '), print_stack(Rest). /*, nl. */

empty_set([]).
member_set([State, Parent, _, _, _], [[State, Parent, _, _, _]|_]).
member_set(X, [_|T]) :- member_set(X, T).

delete_if_in_set(_, [], []).
delete_if_in_set(E, [E|T], T):- !.
delete_if_in_set(E, [H|T], [H|T_new]) :- delete_if_in_set(E, T, T_new), !.
add_if_not_in_set(X, S, S) :- member(X, S), !.
add_if_not_in_set(X, S, [X|S]).
union([], S, S).
union([H|T], S, S_new) :- union(T, S, S2),
		       add_if_not_in_set(H, S2, S_new), !.
subset([], _).
subset([H|T], S) :- member_set(H, S),
	              subset(T, S).
intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-	member_set(H, S),
	intersection(T, S, S_new), !.
intersection([_|T], S, S_new) :- intersection(T, S, S_new), !.
set_difference([], _, []).
set_difference([H|T], S, T_new) :- member_set(H, S),
	set_difference(T, S, T_new), !.
set_difference([H|T], S, [H|T_new]) :- set_difference(T, S, T_new), !.

equal_set(S1, S2) :- subset(S1, S2), subset(S2, S1).

writelist([]) :- nl.
writelist([H|T]):- print(H), tab(1),  /* "tab(n)" skips n spaces. */
                   writelist(T).

empty_pq([]).
insert_pq(State, [], [State]) :- !.
insert_pq(State, [H|Tail], [State, H|Tail]) :- enqueue(X, _, State), enqueue(Y, _, H), precedes(X, Y).
insert_pq(State, [H|T], [H|Tnew]) :- insert_pq(State, T, Tnew).
precedes(X, Y) :- X < Y.
insert_list_pq([ ], L, L).
insert_list_pq([State|Tail], L, New_L) :- insert_pq(State, L, L2), insert_list_pq(Tail, L2, New_L).

member_pq(E, S) :- member(E, S).
insert_sort_queue(State, [], [State]).
insert_sort_queue(State, [H | T], [State, H | T]) :-
    precedes(State, H).
insert_sort_queue(State, [H|T], [H | T_new]) :-
    insert_sort_queue(State, T, T_new).

dequeue_pq(First, [First|Rest], Rest).

test:-go([[2,8,3],[1,0,4],[7,6,5]],[[1,2,3],[8,0,4],[7,6,5]]).

go(Start, Goal) :-
	empty_set(Closed_set),
	empty_pq(Open),
	heuristic(Start, Goal, H),
	insert_pq([Start, nil, 0, H, H], Open, Open_pq),
	path(Open_pq, Closed_set, Goal).

path(Open_pq, _, _) :-
	empty_pq(Open_pq),
	write('Graph searched, no solution found.').

path(Open_pq, Closed_set, Goal) :-
	dequeue_pq([State, Parent, _, _, _], Open_pq, _),
	State = Goal,
	write('The solution path is: '), nl,
	printsolution([State, Parent, _, _, _], Closed_set).

path(Open_pq, Closed_set, Goal) :-
	dequeue_pq([State, Parent, G, H, F], Open_pq, Rest_open_pq),
	write('Selected for Visit: '),
	print(State), nl,
        get_children([State, Parent, G, H, F],
		Rest_open_pq, Closed_set, Children, Goal),
	insert_list_pq(Children, Rest_open_pq, New_open_pq),
	union([[State, Parent, G, H, F]], Closed_set, New_closed_set),
	write('New_open_pq: '),
	print_stack(New_open_pq), nl,
	write('New_closed_set: '),
	writelist(New_closed_set), nl,
        path(New_open_pq, New_closed_set, Goal), !.

get_children([State,_,D,_, _], Rest_open_pq, Closed_set, Children, Goal) :-
     (bagof(Child, moves([State, _, D, _, _], Rest_open_pq, Closed_set, Child, Goal), Children);Children=[]).

moves([State, _, Depth, _, _], Rest_open_pq, Closed_set,
       [Next, State, New_D, H, S], Goal) :-
	move(State, Next),
	not(member_pq([Next, _, _, _, _], Rest_open_pq)),
	not(member_set([Next, _, _, _, _], Closed_set)),
	New_D is Depth + 1,
	heuristic(Next, Goal, H),
	S is New_D + H.

heuristic(Cstate, Goal, H) :- 
    flatten(Cstate, Flat_Cstate),
    flatten(Goal, Flat_Goal),
    count_diff_tiles(Flat_Cstate, Flat_Goal, H).

count_diff_tiles([], [], 0).
count_diff_tiles([H1|T1], [H2|T2], Count) :-
    H1 \= H2,
    count_diff_tiles(T1, T2, TailCount),
    Count is 1 + TailCount.
count_diff_tiles([H1|T1], [H2|T2], Count) :-
    H1 = H2,
    count_diff_tiles(T1, T2, Count).

move(State, Next) :-
    zero_position(State, X, Y),
    move_direction(Direction),
    update_state(State, Direction, X, Y, Next).

zero_position([[0|_]|_], 0, 0).
zero_position([[_|SubRow]|OtherRows], X, Y) :-
    zero_position([SubRow|OtherRows], X2, Y),
    X is X2 + 1.
zero_position([_|OtherRows], X, Y) :-
    zero_position(OtherRows, X, Y2),
    Y is Y2 + 1.

move_direction(left).
move_direction(right).
move_direction(up).
move_direction(down).

update_state(State, left, X, Y, NewState) :-
    Y > 0,
    NewY is Y - 1,
    swap(State, X, Y, X, NewY, NewState).
update_state(State, right, X, Y, NewState) :-
    Y < 2,
    NewY is Y + 1,
    swap(State, X, Y, X, NewY, NewState).
update_state(State, up, X, Y, NewState) :-
    X > 0,
    NewX is X - 1,
    swap(State, X, Y, NewX, Y, NewState).
update_state(State, down, X, Y, NewState) :-
    X < 2,
    NewX is X + 1,
    swap(State, X, Y, NewX, Y, NewState).

swap(State, OldX, OldY, NewX, NewY, NewState) :-
    replace_element(State, OldX, OldY, NewX, NewY, TempState),
    replace_element(TempState, NewX, NewY, OldX, OldY, NewState).

replace_element(State, X, Y, NewX, NewY, NewState) :-
    replace_nth(State, X, OldRow, NewState, NewRow),
    replace_nth(OldRow, Y, _, NewRow, Element),
    nth0(NewY, NewState, ReplacementRow),
    nth0(Element, ReplacementRow).

replace_nth([_|T], 0, X, [X|T]).
replace_nth([H|T], I, X, [H|R]) :-
    I > 0,
    NI is I - 1,
    replace_nth(T, NI, X, R).

print([]) :- nl.
print([H|T]) :-
    print(H),
    print(T).
print([H|T]) :-
    print(H), tab(1),
    print(T).
print(H) :-
    write(H).

printsolution([State, nil, _, _, _], _) :-
	print(State), nl.
printsolution([State, Parent, _, _, _], Closed_set) :-
	member([Parent, Grandparent, _, _, _], Closed_set),
	printsolution([Parent, Grandparent, _, _, _], Closed_set),
	print(State), nl.
