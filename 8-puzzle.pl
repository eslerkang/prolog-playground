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
/* member_set(E, S) :- member(E, S). */
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

s_difference([], _, []).
s_difference([H|T], [HS|HT], T_new) :- H = HS,
	s_difference(T, HT, T_new), !.
s_difference([H|T], [_|HT], [H|T_new]) :- s_difference(T, HT, T_new), !.

count_differences(Matrix1, Matrix2, Count) :-
    flatten(Matrix1, Flat1),
    flatten(Matrix2, Flat2),
    count_differences_list(Flat1, Flat2, 0, Count).

count_differences_list([], [], Count, Count).
count_differences_list([H1|T1], [H2|T2], Acc, Count) :-
    (H1 \= H2 ->
        NewAcc is Acc + 1;
        NewAcc is Acc),
    count_differences_list(T1, T2, NewAcc, Count).

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

% test :- go([[1, 2, 3], [8, 0, 4], [7, 6, 5]], [[0, 1, 2], [8, 6, 3], [7, 5, 4]]), !.
% 7 / 11

% test :- go([[1, 2, 3], [8, 0, 4], [7, 6, 5]], [[1, 2, 3], [4, 5, 6], [7, 8, 0]]), !.
% unsolvable.

test :- go([[7, 2, 4], [5, 0, 6], [8, 3, 1]], [[0, 1, 2], [3, 4, 5], [6, 7, 8]]), !.
% 16 / 0

% test :- go([[2, 8, 3], [1, 6, 4], [7, 0, 5]], [[1, 2, 3], [8, 0, 4], [7, 6, 5]]), !.
% 11 / 7

count_inversions([], 0).
count_inversions([H|T], Count) :-
    findall(X, (member(X, T), X < H, H\=0, X\=0), Smaller),
    length(Smaller, NumSmaller),
    count_inversions(T, CountT),
    Count is NumSmaller + CountT.

is_solvable(Start, Goal) :-
	flatten(Start, FStart),
	flatten(Goal, FGoal),
	count_inversions(FStart, IStart),
	count_inversions(FGoal, IGoal),
	Diff is (IStart - IGoal) mod 2,
	Diff \= 1.

go(Start, Goal) :-
	(is_solvable(Start, Goal) -> true ; write('Cannot solve.'), false),
	empty_set(Closed_set),
	empty_pq(Open),
	heuristic(Start, Goal, H),
	insert_pq([Start, nil, 0, H, H], Open, Open_pq),
	path(Open_pq, Closed_set, Goal, 0).

path(Open_pq, _, _, _) :-
	empty_pq(Open_pq),
	write('Graph searched, no solution found.').

path(Open_pq, Closed_set, Goal, _) :-
	dequeue_pq([State, Parent, _, _, _], Open_pq, _),
	State = Goal,
	write('The solution path is: '), nl,
	printsolution([State, Parent, _, _, _], Closed_set).

path(Open_pq, Closed_set, Goal, S) :-
	dequeue_pq([State, Parent, G, H, F], Open_pq, Rest_open_pq),
	write(S), nl,
	% write('Selected for Visit: '),
	% print(State), nl,
  get_children([State, Parent, G, H, F], Rest_open_pq, Closed_set, Children, Goal),
	insert_list_pq(Children, Rest_open_pq, New_open_pq),
	union([[State, Parent, G, H, F]], Closed_set, New_closed_set),
	%write('New_open_pq: '),
	%print_stack(New_open_pq), nl,
	%write('New_closed_set: '),
	%writelist(New_closed_set), nl,
	NS is S+1,
  path(New_open_pq, New_closed_set, Goal, NS), !.


get_children([State,_,D,_, _], Rest_open_pq, Closed_set, Children, Goal) :-
     (bagof(Child, moves([State, _, D, _, _], Rest_open_pq, Closed_set, Child, Goal), Children);Children=[]).
		 	% write('New_children: '),
			% print_stack(Children), nl.

moves([State, _, Depth, _, _], Rest_open_pq, Closed_set,
       [Next, State, New_D, H, S], Goal) :-
	move(State, Next),
	not(member_pq([Next, _, _, _, _], Rest_open_pq)),
	not(member_set([Next, _, _, _, _], Closed_set)),
	New_D is Depth + 1,
	heuristic(Next, Goal, H),		% application specific
	S is New_D + H.
	%, write(Next), nl.

heuristic(Cstate, Goal, 0) :- Cstate = Goal, !.
heuristic(State, Goal, H) :-
	count_differences(State, Goal, H).

printsolution([State, nil, _, _, _], _) :- write(State), nl.
printsolution([State, Parent, _, _, _], Closed_set) :-
	member_set([Parent, Grandparent, _, _, _], Closed_set),
	printsolution([Parent, Grandparent, _, _, _], Closed_set),
	write(State), nl.

zero_position([Row|_], X, Y) :-
	nth0(Y, Row, 0),
	X is 0.

zero_position([_|Rest], X, Y) :-
	zero_position(Rest, X1, Y),
	X is X1 + 1.

move(State, Next) :-
	zero_position(State, X, Y),
	move_to(State, X, Y, Next).

move_to(State, X, Y, Next) :-
	Y > 0,
	NewY is Y - 1,
	swap(State, X, Y, X, NewY, Next).

move_to(State, X, Y, Next) :-
	Y < 2,
	NewY is Y + 1,
	swap(State, X, Y, X, NewY, Next).

move_to(State, X, Y, Next) :-
	X > 0,
	NewX is X - 1,
	swap(State, X, Y, NewX, Y, Next).

move_to(State, X, Y, Next) :-
	X < 2,
	NewX is X + 1,
	swap(State, X, Y, NewX, Y, Next).

replace_in_list([_|Tail], 0, E, [E|Tail]) :- !.

replace_in_list([H|Tail], Index, E, [H|ResultTail]) :-
	NextIndex is Index - 1,
	replace_in_list(Tail, NextIndex, E, ResultTail), !.

replace_in_matrix(Matrix, X, Y, E, ResultMatrix) :-
	nth0(X, Matrix, Row),
	replace_in_list(Row, Y, E, ResultRow),
	replace_in_list(Matrix, X, ResultRow, ResultMatrix).

swap(State, X, Y, NewX, NewY, Next) :-
	nth0(NewX, State, Row),
	nth0(NewY, Row, E),
	replace_in_matrix(State, X, Y, E, TempState),
	replace_in_matrix(TempState, NewX, NewY, 0, Next).