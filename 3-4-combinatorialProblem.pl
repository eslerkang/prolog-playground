:- use_module(library(clpb)).

% sat(A =:= ~A+B).
% sat(((~D)+(~B)+C)*(B+(~A)+(~C))*((~C)+(~B)+E)*(E+(~D)+B)*(B+E+(~C))).
% sat(((~D)+(~B)+C)*(B+(~A)+(~C))*((~C)+(~B)+E)*(E+(~D)+B)*(B+E+(~C))), labeling([A,B,C,D,E]).
% sat(~A*A).
% sat((~A+B)*A*(~B)).