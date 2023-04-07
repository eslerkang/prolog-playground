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

path2(X,Y):-move(X,Z),move(Z,Y).
path3(X,Y):-move(X,Z),!,move(Z,Y).
pathN(X,Y):-move(X,Z),move(Z,Y),!.
path4(Z,Z,_).
path4(X,Z,L):-move(X,Y),not(member(Y,L)),!,path4(Y,Z,[Y|L]).
path5(Z,Z,L):-writelist(L).
path5(X,Z,L):-move(X,Y),not(member(Y,L)),path5(Y,Z,[Y|L]),!.
path6(Z,Z,L):-writelist(L).
path6(X,Z,L):-move(X,Y),not(member(Y,L)),path6(Y,Z,[Y|L]).

writelist([]).
writelist([H|T]):- writelist(T), print(H), write(' ').
