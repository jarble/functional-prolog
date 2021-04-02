:- initialization(main).
:- set_prolog_flag('double_quotes','chars').
:- use_module(library(chr)).
:- chr_constraint functional_syntax/1.

main :- isolate_var(B+1=A,A,C),writeln(C).

isolate_var(A+B,B1).
isolate_var(B=A,A,C) :-
	var(A),
	isolate_var(A=B,A,C).
isolate_var(A=B,A,A=B) :-
	var(A),
	occurrences_of_var(A,B,0).
simplify(A+B,C) :-
	ground(A+B),
	simplify(A,A1),
	simplify(B,B1).
simplify([],[]).
simplify([A|B],[A1|B1]) :-
	simplify(A,A1),simplify(B,B1).
