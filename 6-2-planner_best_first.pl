
%%%%%%%%% Simple Prolog Planner %%%%%%%%
%%%
%%% This is one of the example programs from the textbook:
%%%
%%% Artificial Intelligence:
%%% Structures and strategies for complex problem solving
%%%
%%% by George F. Luger and William A. Stubblefield
%%%
%%% Corrections by Christopher E. Davis (chris2d@cs.unm.edu)
%%%
%%% These programs are copyrighted by Benjamin/Cummings Publishers.
%%%
%%% We offer them for use, free of charge, for educational purposes only.
%%%
%%% Disclaimer: These programs are provided with no warranty whatsoever as to
%%% their correctness, reliability, or any other property.  We have written
%%% them for specific educational purposes, and have made no effort
%%% to produce commercial quality computer programs.  Please do not expect
%%% more of them then we have intended.
%%%
%%% This code has been tested with SWI-Prolog (Multi-threaded, Version 5.2.13)
%%% and appears to function as intended.

member(X,[X|_]).
member(X,[_|T]):-member(X,T).

empty_stack([]).

    % member_stack tests if an element is a member of a stack

member_stack(E, S) :- member(E, S).

    % stack performs the push, pop and peek operations
    % to push an element onto the stack
        % ?- stack(a, [b,c,d], S).
    %    S = [a,b,c,d]
    % To pop an element from the stack
    % ?- stack(Top, Rest, [a,b,c]).
    %    Top = a, Rest = [b,c]
    % To peek at the top element on the stack
    % ?- stack(Top, _, [a,b,c]).
    %    Top = a

stack(E, S, [E|S]).

%%%%%%%%%%%%%%%%%%%% queue operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple, list based implementation of
    % FIFO queues

    % empty queue generates/tests an empty queue

    % member_queue tests if an element is a member of a queue

%member_queue(E, S) :- member(E, S).

    % add_to_queue adds a new element to the back of the queue

add_to_queue(E, [], [E]).
add_to_queue(E, [H|T], [H|Tnew]) :- add_to_queue(E, T, Tnew).

    % remove_from_queue removes the next element from the queue
    % Note that it can also be used to examine that element
    % without removing it

remove_from_queue(E, [E|T], T).

append_queue(First, Second, Concatenation) :-
    append(First, Second, Concatenation).

%%%%%%%%%%%%%%%%%%%% set operations %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % These predicates give a simple,
    % list based implementation of sets

    % empty_set tests/generates an empty set.

empty_set([]).

member_set(E, S) :- member(E, S).

    % add_to_set adds a new member to a set, allowing each element
    % to appear only once

add_to_set(X, S, S) :- member(X, S).
add_to_set(X, S, [X|S]).

remove_from_set(_, [], []).
remove_from_set(E, [E|T], T).
remove_from_set(E, [H|T], [H|T_new]) :-
    remove_from_set(E, T, T_new), !.

union([], S, S).
union([H|T], S, S_new) :-
    union(T, S, S2),
    add_to_set(H, S2, S_new).

intersection([], _, []).
intersection([H|T], S, [H|S_new]) :-
    member_set(H, S),
    intersection(T, S, S_new),!.
intersection([_|T], S, S_new) :-
    intersection(T, S, S_new),!.

set_diff([], _, []).
set_diff([H|T], S, T_new) :-
    member_set(H, S),
    set_diff(T, S, T_new),!.
set_diff([H|T], S, [H|T_new]) :-
    set_diff(T, S, T_new), !.

plan_subset([], _).
plan_subset([H|T], S) :-
    member_set(H, S),
    plan_subset(T, S).

equal_set(S1, S2) :-
    plan_subset(S1, S2), plan_subset(S2, S1).

%%%%%%%%%%%%%%%%%%%%%%% priority queue operations %%%%%%%%%%%%%%%%%%%

    % These predicates provide a simple list based implementation
    % of a priority queue.

    % They assume a definition of precedes for the objects being handled
empty_queue([]).
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).
dequeue(E, [E|T], T).
dequeue(E, [E|T], _).
member_queue(Element, Queue) :- member(Element, Queue).
add_list_to_queue(List, Queue, Newqueue) :- append(Queue, List, Newqueue).
dequeue_pq(First, [First|Rest], Rest).

empty_pq([]).
insert_pq(State, [], [State]) :- !.
insert_pq(State, [H|Tail], [State, H|Tail]) :- enqueue(X, _, State), enqueue(Y, _, H), precedes(X, Y).
insert_pq(State, [H|T], [H|Tnew]) :- insert_pq(State, T, Tnew).
precedes(X, Y) :- X < Y.
insert_list_pq([ ], L, L).
insert_list_pq([State|Tail], L, New_L) :- insert_pq(State, L, L2), insert_list_pq(Tail, L2, New_L).

empty_sort_queue([]).

member_sort_queue(E, S) :- member(E, S).

insert_sort_queue(State, [], [State]).
insert_sort_queue(State, [H | T], [State, H | T]) :- precedes(State, H).
%precedes(X, Y) :- X < Y.
insert_sort_queue(State, [H|T], [H | T_new]) :- insert_sort_queue(State, T, T_new).
remove_sort_queue(First, [First|Rest], Rest).

find_parent([X,Y,A|_],C):- C=[H|T], H=[ST|_], equal_set(X,ST), member_set([ST,Y,A|_],C).
find_parent([X,Y,A|_],C):- C=[_|T], find_parent([X,Y,A|_],T).

printsolution([_, nil, nil, _, _, _], _).
printsolution(Pat, Closed_set) :-
        Pat = [_, Parent, M, _, _, _],
        find_parent([Parent, Grandparent, L, _, _, _], Closed_set),
        NP = [Parent, Grandparent, L, _, _, _],
	printsolution(NP, Closed_set),
	write(M), nl.

member_state(S, [H|_]) :- equal_set(S, H).
member_state(S, [_|T]) :- member_state(S, T).
member_set_pq([Next|_], [H|T]) :- H=[X|_], equal_set(Next,X).
member_set_pq([Next|_],[_|T]) :- member_set_pq([Next|_], T).

reverse_print_stack(S) :-	empty_stack(S).
reverse_print_stack(S) :-	stack(E, Rest, S),
				reverse_print_stack(Rest),
				write(E), nl.

print_stack_actions(S):- empty_stack(S).
print_stack_actions(S):- stack(E, Rest, S), E=[_,_,P,_,_,_],write(P), write(' '), print_stack_actions(Rest). /*, nl. */

print_stack_states(S):- empty_stack(S).
print_stack_states(S):- stack(E, Rest, S), E=[P,_,_,_,_,_],write(P), write(' '), print_stack_states(Rest). /*, nl. */

writelist([]) :- nl.
writelist([H|T]):- print(H), tab(1),  /* "tab(n)" skips n spaces. */
                   writelist(T).


plan(Open_pq, _, _) :-
	empty_pq(Open_pq),
	write('No solution found.').

plan(Open_pq, Closed_set, Goal) :-
	dequeue_pq(Pattern, Open_pq, _),
        Pattern = [State, Parent, M, _, _, _],
	equal_set(State, Goal),
	write('The solution path is: '), nl,
	printsolution([State, Parent, M, _, _, _], Closed_set).

plan(Open_pq, Closed_set, Goal) :-
	dequeue_pq([State, Parent, M, G, H, F], Open_pq, Rest_open_pq),
        get_children([State, Parent, M, G, H, F], Rest_open_pq, Closed_set, Children, Goal),
	insert_list_pq(Children, Rest_open_pq, New_open_pq),
	union([[State, Parent, M, G, H, F]], Closed_set, New_closed_set),
        plan(New_open_pq, New_closed_set, Goal), !.

get_children([State, _, _, D, _, _], Rest_open_pq, Closed_set, Children, Goal) :-
     (bagof(Child, moves([State, _, _, D, _, _], Rest_open_pq, Closed_set, Child, Goal), Children);Children=[]).

moves([State, _, _, Depth, _, _], Rest_open_pq, Closed_set, Child, Goal) :-
        movable(Name,State,Next),
        not(member_set_pq([Next, _, _, _, _, _], Rest_open_pq)),
	not(member_set_pq([Next, _, _, _, _, _], Closed_set)),
        New_D is Depth + 1,
	heuristic(Next, Goal, H),		% application specific
	F is New_D + H,
        Child = [Next, State, Name, New_D, H, F]. %, write(Next), nl.


change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	change_state(S, T, S2),
					add_to_set(P, S2, S_new).
change_state(S, [del(P)|T], S_new) :-	change_state(S, T, S2),
					remove_from_set(P, S2, S_new).
conditions_met(P, S) :- plan_subset(P, S).

/* sample moves */

movable(X,From,To):-move(X,Y,Z),conditions_met(Y,From),change_state(From,Z,To).

move(pickup(X), [handempty, clear(X), on(X,Y)], [del(handempty), del(clear(X)), del(on(X,Y)), add(clear(Y)), add(holding(X))]).

move(pickup(X), [handempty, clear(X), ontable(X)], [del(handempty), del(clear(X)), del(ontable(X)), add(holding(X))]).

move(putdown(X), [holding(X)], [del(holding(X)), add(ontable(X)), add(clear(X)), add(handempty)]).

move(stack(X,Y), [holding(X), clear(Y)], [del(holding(X)), del(clear(Y)), add(handempty), add(on(X,Y)), add(clear(X))]).

go(S, G) :-
	empty_set(Closed_set),
	empty_pq(Open),
	heuristic(S, G, H),
	insert_pq([S, nil, nil, 0, H, H], Open, Open_pq),
	plan(Open_pq, Closed_set, G).

heuristic(Cstate, Goal, 0) :- Cstate = Goal, !.
heuristic(S, G, H) :- intersection(S, G, I), length(G, GL), length(I, IL), H is GL - IL.

test :- go([handempty, ontable(b), ontable(c), on(a, b), clear(c), clear(a)],
	          [handempty, ontable(c), on(a,b), on(b,c), clear(a)]).

test2 :- go([handempty, ontable(a), ontable(c), on(b, a), clear(c), clear(b)],
	          [handempty, ontable(c), on(a,b), on(b,c), clear(a)]).

test3 :- go([handempty, ontable(a), ontable(c), ontable(d), on(b,a), on(e,d), clear(b), clear(c), clear(e)],
	          [handempty, ontable(e), on(a,b), on(b,c), on(c,d), on(d,e), clear(a)]).




