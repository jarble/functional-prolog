:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

unify_if_subsumes(A,B) :- subsumes_term(A,B),A=B.

define_function(add(A,B) :-
	A+B
).

functional_syntax(A,A) :- atom(A);number(A).
functional_syntax(A) :- functional_syntax(A,_).
functional_syntax(Input,true) :- unify_if_subsumes({A},Input),call(A).
functional_syntax(Input,Output) :- unify_if_subsumes(A+B,Input),functional_syntax(A,A1),functional_syntax(B,B1),Output is A1+B1.
functional_syntax(Input,true) :- unify_if_subsumes(writeln(A),Input), functional_syntax(A,A1), writeln(A1).

main :- functional_syntax(writeln(add(1,2))).
