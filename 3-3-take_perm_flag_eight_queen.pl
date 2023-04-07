take([H|T],H,T).
take([H|T],R,[H|S]):-take(T,R,S).

perm([],[]).
perm(List,[H|T]):-take(List,H,R),perm(R,T).

checkOrder(A,B):-member(A,[red]),member(B,[red,white,blue]).
checkOrder(A,B):-member(A,[white]),member(B,[white,blue]).
checkOrder(A,B):-member(A,[blue]),member(B,[blue]).
checkColours([_|T]):-T==[].
checkColours([H1,H2|T]):-checkOrder(H1,H2), checkColours([H2|T]).
flag(In, Out):-perm(In, Out),checkColours(Out).

eightqueens(R):-perm([1,2,3,4,5,6,7,8],R), checkDiagonals(R), printR(R).

checkDiagonals([_]).
checkDiagonals([H|T]):-rchkDiag([H|T],1),checkDiagonals(T).

rchkDiag([_],_).
rchkDiag([H1,H2|T],ACC):-H1=\=H2+ACC,H1=\=H2-ACC,rchkDiag([H1|T],ACC+1).

printR([]).
printR([H|T]):-printLine(H), printR(T).
 
printLine(1):-print('X O O O O O O O'),nl.
printLine(2):-print('O X O O O O O O'),nl.
printLine(3):-print('O O X O O O O O'),nl.
printLine(4):-print('O O O X O O O O'),nl.
printLine(5):-print('O O O O X O O O'),nl.
printLine(6):-print('O O O O O X O O'),nl.
printLine(7):-print('O O O O O O X O'),nl.
printLine(8):-print('O O O O O O O X'),nl.