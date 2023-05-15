:- use_module(library(clpfd)).
% clpfd -> finite domain

puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
	Vars = [S,E,N,D,M,O,R,Y],
	Vars ins 0..9,
	all_different(Vars),
	S*1000 + E*100 + N*10 + D + M*1000 + O*100 + R*10 + E #=
	M*10000 + O*1000 + N*100 + E*10 + Y,
	M #\= 0, S #\= 0.

mypuzzle([T,W,O] + [T,W,O] = [F,O,U,R]) :-
	Vars = [F,O,R,T,U,W],
	Vars ins 0..9,
	all_different(Vars),
	T*200 + W*20 + O*2 #= F*1000 + O*100 + U*10 + R,
	F #\= 0.

mypuzzle2([T,W,O] + [T,W,O] = [F,O,U,R]) :-
	Vars = [F,O,R,T,U,W],
	[X1,X2,X3] ins 0..1,
	Vars ins 0..9,
	all_different(Vars),
	O + O #= R + 10*X1,
	X1 + W + W #= U + 10*X2,
	X2 + T + T #= O + 10*X3,
	X3 #= F, F #\= 0.

%puzzle(As + Bs = Cs), labeling([],As).

sol1([S,E,N,D,M,O,R,Y]):-puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]).
sol2([F,O,R,T,U,W]):-mypuzzle([T,W,O] + [T,W,O] = [F,O,U,R]).
sol3([F,O,R,T,U,W]):-mypuzzle2([T,W,O] + [T,W,O] = [F,O,U,R]).