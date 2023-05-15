empty_queue([]).
enqueue(E, [], [E]).
enqueue(E, [H|T], [H|Tnew]) :- enqueue(E, T, Tnew).
dequeue(E, [E|T], T).
member_queue(Element, Queue) :- member(Element, Queue).
add_list_to_queue(List, Queue, Newqueue) :- append(Queue, List, Newqueue).
