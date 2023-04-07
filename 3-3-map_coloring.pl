colorable :- diff(WA,NT), diff(WA,SA),	diff(NT,QU), diff(NT,SA), diff(QU,NSW), diff(QU,SA), diff(NSW,V), diff(NSW,SA), diff(V, SA),
write('WA= '), write(WA), write(' NT= '), write(NT), write(' SA= '), write(SA), write(' QU= '), write(QU), write(' NSW= '), write(NSW), write(' V= '), write(V), nl, fail.
diff(red,blue).
diff(red,green). 
diff(green,red).
diff(green,blue). 
diff(blue,red).
diff(blue,green).
