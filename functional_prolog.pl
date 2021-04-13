:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :- functional_syntax((
			A = factorial(1),
			(A==1,writeln(yes);A==2,writeln('no')),
			writeln(factorial(3))
		)).

functional_syntax(writeln(A) :-(
	A1=A,{writeln(A1)})
).

functional_syntax(factorial(N) :- (
	N=1 -> 1; N*factorial(N-1)
)).

unify_if_subsumes(A,B) :- subsumes_term(A,B),A=B.

functional_syntax(A) :- functional_syntax(A,true).
functional_syntax(A,A) :- number(A);var(A);atom(A);A==[].
functional_syntax(Input,true) :- unify_if_subsumes({A},Input),A.

functional_syntax(Input,C) :-
	ground(Input),
	Input =.. [Name|Params],
    member1(Name,[+,-,*,/,^,**]),
    functional_syntax(Params,Params1),
    Output =.. [Name|Params1],
    C is Output.

functional_syntax(Input,C) :-
	Input =.. [Name|Params],
    member1(Name,[=,==,\=,>,<,>=,=<,not]),
    % writeln(Input),
    functional_syntax(Params,Params1),
    Output =.. [Name|Params1],
    (Output,C=true;(\+Output),C=false).

functional_syntax(Input,Output) :-
	unify_if_subsumes((A->B;C),Input),
	functional_syntax((A1=A,(A1==true,Output=B;A1==false,Output=C))).

functional_syntax(Input,[A1|B1]) :-
	unify_if_subsumes([A|B],Input),
    functional_syntax(A,A1),functional_syntax(B,B1).
functional_syntax(Input,Result) :-
	unify_if_subsumes((A,B),Input),
	functional_syntax(A,A1),
	((A1==false,Result=false);
	functional_syntax(B,B1),(B1==false,Result=false;B1==true,Result=true)
	).
functional_syntax(Input,Result) :-
	unify_if_subsumes((A;B),Input),functional_syntax(A,A1),
	(A1==true,Result=true;A1==false,functional_syntax(B,Result)).

functional_syntax(Input,Output1) :-
	functional_syntax((A :- B)),
	subsumes_term(A,Input),
	A=Input,
	% writeln((A:-B)),
	functional_syntax(B,Output1).

member1(El, [H|T]) :-
  member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
	member_(T, El, H).
