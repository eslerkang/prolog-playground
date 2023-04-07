frame(name(bird), isa(animal), [travel(flies), feathers],[]).
frame(name(penguin), isa(bird), [color(brown)], [travel(walks)]).
frame(name(canary), isa(bird), [color(yellow), call(sing)], [size(small)]).
frame(name(tweety), isa(canary), [], [color(white)]).
frame(name(opus), isa([penguin, cartoon_char]), [color(black)],	[]).
frame(name(cartoon_char), isa(char), [color(red)],[]).
get(Prop, Object) :- frame(name(Object), _, List_of_properties, _),
	member(Prop, List_of_properties).
get(Prop, Object) :- frame(name(Object), _, _, List_of_defaults),
	member(Prop, List_of_defaults).
get(Prop, Object) :- frame(name(Object), isa(Parent), _, _),
	get(Prop, Parent).
get(Prop, Object) :- frame(name(Object), isa(List), _, _),
	get_multiple(Prop, List).
get_multiple(Prop, [Parent|_]) :- get(Prop, Parent).
get_multiple(Prop, [_|Rest]) :- get_multiple(Prop, Rest),!.
