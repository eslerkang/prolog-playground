empty_stack([]).
stack(Top, Stack, [Top|Stack]).
member_stack(Element, Stack):-member(Element, Stack).
add_list_to_stack(List, Stack, Result):-append(List, Stack, Result).
reverse_print_stack(S):-empty_stack(S).
reverse_print_stack(S):-stack(E, Rest, S), reverse_print_stack(Rest),	write(E), nl.
