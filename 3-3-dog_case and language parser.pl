
collie(fred).
master(fred,sam).
day(saturday).
not(warm(saturday)).
trained(fred).
spaniel(happy).
gooddog(X):-spaniel(X).
gooddog(X):-collie(X),trained(X).
location(X,Z):-gooddog(X),master(X,Y),location(Y,Z).
location(sam,museum):-day(saturday),not(warm(saturday)).

n(man).
n(dog).
art(a).
art(the).
v(likes).
v(bites).
np([X]):-n(X).
np(X):-art(Y),n(Z),append([Y],[Z],X).
vp([X]):-v(X).
vp(X):-v(Y),np(Z),append([Y],Z,X).
sentence(X):-np(Y),vp(Z),append(Y,Z,X).
