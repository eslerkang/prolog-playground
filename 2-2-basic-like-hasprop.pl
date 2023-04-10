% dynamic하게 likes를 사용할 것. 인자는 2개라는 뜻.
:- dynamic likes/2.

likes(george, kate).
likes(george, susie).
likes(george, wine).
likes(susie, wine).
likes(kate, gin).
likes(kate, susie).

friends(X, Y) :-likes(X, Z), likes(Y, Z), X \= Y.

isa(canary, bird).
isa(robin, bird).
isa(ostrich, bird).
isa(penguin, bird).
isa(bird, animal).
isa(fish, animal).
isa(opus, penguin).
isa(tweety, canary).
hasprop(robin, sound, sing).
hasprop(fish, travel, swim).
hasprop(ostrich, travel, walk).
hasprop(bird, travel, fly).
hasprop(penguin, color, brown).
hasprop(canary, color, yellow).
hasprop(robin, color, red).
hasprop(tweety, color, white).
hasprop(canary, sound, sing).
hasprop(bird, cover, feathers).
hasprop(animal, cover, skin).

hasproperty(Object, Property, Value) :- hasprop(Object, Property, Value).
hasproperty(Object, Property, Value) :- isa(Object, Parent), hasproperty(Parent, Property, Value).
