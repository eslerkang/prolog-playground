:-dynamic sol/1.

empty_stack([]).
stack(Top, Stack, [Top|Stack]).
member_stack(Element, Stack):-member(Element, Stack).
add_list_to_stack(List, Stack, Result):-append(List, Stack, Result).

test:-go(state(w,w,w,w),state(e,e,e,e)).

go(Start,Goal) :-
	empty_stack(Empty_been_stack),
	stack(Start,Empty_been_stack,Been_stack),
	path(Start,Goal,Been_stack).

path(Goal,Goal,Been_stack):-assert(sol(Been_stack)).

path(State,Goal,Been_stack):-
	move(State,Next_state),
	not(member_stack(Next_state,Been_stack)),
	stack(Next_state,Been_stack,New_been_stack),
	path(Next_state,Goal,New_been_stack),!.

move(state(X,X,G,C), state(Y,Y,G,C))
              :- opp(X,Y), not(unsafe(state(Y,Y,G,C))).

move(state(X,W,X,C), state(Y,W,Y,C))
              :- opp(X,Y), not(unsafe(state(Y,W,Y,C))).

move(state(X,W,G,X), state(Y,W,G,Y))
              :- opp(X,Y), not(unsafe(state(Y,W,G,Y))).

move(state(X,W,G,C), state(Y,W,G,C))
              :- opp(X,Y), not(unsafe(state(Y,W,G,C))).

move(state(F,W,G,C), state(F,W,G,C))
              :- fail.

unsafe(state(X,Y,Y,_)):- opp(X,Y).
unsafe(state(X,_,Y,Y)):- opp(X,Y).

opp(e,w).
opp(w,e).












































