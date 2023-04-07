last([H],H).
last([_|T],H):-last(T,H).

