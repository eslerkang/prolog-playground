:- dynamic tab/1.

tab(0) :- !.
tab(X) :- X \== 0, write('\x2001'), Y is X-1, tab(Y).

empty_queue([]).
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).
dequeue(E, [E|T], T).
dequeue(E, [E|T], _).
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
member_set([State, Parent], [[State, Parent]|_]).
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

printsolution([State, nil], _):-write(State), nl.
printsolution([State, Parent], Closed_set) :-
		member_set([Parent, Grandparent], Closed_set),
		printsolution([Parent, Grandparent], Closed_set),
		write(State), nl.


test:-go(state(w,w,w,w),state(e,e,e,e)).

go(Start, Goal):-empty_queue(Empty_open_queue),
	enqueue([Start, nil], Empty_open_queue, Open_queue),
	empty_set(Closed_set),
	path(Open_queue, Closed_set, Goal).

path(Open_queue, _, _):- empty_queue(Open_queue),
	write('No solution found with these rules').

path(Open_queue, Closed_set, Goal):- dequeue([State, Parent], Open_queue, _),
	State = Goal,
	write('A Solution is Found!'), nl,
	printsolution([State, Parent], Closed_set).

path(Open_queue, Closed_set, Goal) :-
	dequeue([State, Parent], Open_queue, Rest_open_queue),
	write('Selected for Visit: '),
	print(State), nl,
	get_children(State, Rest_open_queue, Closed_set, Children),
	add_list_to_queue(Children, Rest_open_queue, New_open_queue),
	union([[State, Parent]], Closed_set, New_closed_set),
	write('New_open_queue: '),
	print_stack(New_open_queue), nl,
	write('New_closed_set: '),
	writelist(New_closed_set), nl,
	path(New_open_queue, New_closed_set, Goal), !.

get_children(State, Rest_open_queue, Closed_set, Children):-
		(bagof(Child, moves(State, Rest_open_queue, Closed_set, Child), Children);Children=[]).

moves(State, Rest_open_queue, Closed_set, [Next, State]) :-
		move(State, Next),
		not(unsafe(Next)),	% test depends on problem
		not(member_queue([Next,_], Rest_open_queue)),
		not(member_set([Next,_], Closed_set)).

move(state(X,X,G,C), state(Y,Y,G,C)):- opp(X,Y).

move(state(X,W,X,C), state(Y,W,Y,C)):- opp(X,Y).

move(state(X,W,G,X), state(Y,W,G,Y)):- opp(X,Y).

move(state(X,W,G,C), state(Y,W,G,C)):- opp(X,Y).

unsafe(state(X,Y,Y,_)):- opp(X,Y).
unsafe(state(X,_,Y,Y)):- opp(X,Y).

opp(e,w).
opp(w,e).





