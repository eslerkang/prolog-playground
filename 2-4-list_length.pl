len([],0).
len([_|T],N):-len(T,M),N is M+1.

len2([],Acc,Acc).
len2([_|Tail],Acc,Result):-
	AccNext is Acc + 1,
	len2(Tail,AccNext,Result).

len2(List,Result):-len2(List,0,Result).

biglist(0,[]).
biglist(N,[N|T]):-
	M is N-1,
	biglist(M,T),
	M=M.

biglist2(0,[]).
biglist2(N,[N|T]):-
	M is N-1,
	M=M,
	biglist2(M,T).
