:- dynamic been/1.

move(1,6).
move(1,8).
move(2,7).
move(2,9).
move(3,8).
move(3,4).
move(4,9).
move(4,3).
move(6,7).
move(6,1).
move(7,6).
move(7,2).
move(8,3).
move(8,1).
move(9,4).
move(9,2).
opath(Z,Z,L):-writelist(L).
opath(X,Y,L):-move(X,Z),opath(Z,Y,[Z|L]).

beenpath(Z,Z,L):-writelist(L).
beenpath(X,Y,L):-move(X,Z), not(been(Z)), assert(been(Z)), beenpath(Z,Y,[Z|L]).

path(Z,Z,L):-writelist(L).
path(X,Y,L):-move(X,Z),not(member(Z,L)),path(Z,Y,[Z|L]).

cutpath(Z,Z,L):-writelist(L).
cutpath(X,Y,L):-move(X,Z),not(member(Z,L)),cutpath(Z,Y,[Z|L]),!.

writelist([]).
writelist([H|T]):- writelist(T), print(H), write(' ').
