:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :- functional_syntax((
			L=2-1,
			writeln(L),
			A = factorial(1),
			(A==1,writeln('yes');A==2,writeln('no')),
			writeln(factorial(3)),
			writeln([factorial(3),factorial(4),call(factorial,[5]),sin(100)])
		)).

functional_syntax(writeln(A) :-(
	A1=A,{writeln(A1)})
).

functional_syntax(factorial(N) :- (
	N=1 -> 1; N*factorial(N-1)
)).
functional_syntax(A) :- functional_syntax(A,true).

functional_syntax(Input,Output) :-
	clause(functional_syntax_(Input1,Output),_),
	unify_if_subsumes(Input1,Input),
	functional_syntax_(Input1,Output).


functional_syntax(A,A) :- number(A);var(A);atom(A);A==[].


functional_syntax(call(A,B),Output) :-
	functional_syntax(B,B1),
	A1 =.. [A|B1],
	functional_syntax(A1,Output).

functional_syntax(Input,C) :-
	ground(Input),
	Input =.. [Name|Params],
    member1(Name,[+,-,*,/,^,**,sin,cos,tan,asin,acos,atan,sinh,cosh,tanh,asinh,acosh,atanh,log,log10,exp,pi]),
    functional_syntax(Params,Params1),
    Output =.. [Name|Params1],
    C is Output.

functional_syntax(Input,C) :-
	Input =.. [Name|Params],
    member1(Name,[=,==,\=,>,<,>=,=<,not]),
    
    functional_syntax(Params,Params1),
    % writeln(Input),
    Output =.. [Name|Params1],
    (Output,C=true;(\+Output),C=false).

functional_syntax(Input,Output) :-
	functional_syntax(A :- _),
	unify_if_subsumes(A,Input),
	A =.. [Name,Params],
	functional_syntax(Params,Params1),
	A1 =.. [Name,Params1],
	functional_syntax(A1 :- B1),
	% writeln((A1:-B1)),
	functional_syntax(B1,Output).
	
functional_syntax_({A},true) :- A.
functional_syntax_((A->B;C),Output) :-
	functional_syntax((A1=A,(A1==true,Output=B;A1==false,Output=C))).
functional_syntax_([A|B],[A1|B1]) :-
    functional_syntax(A,A1),functional_syntax(B,B1).
functional_syntax_((A,B),Result) :-
	functional_syntax(A,A1),
	((A1==false,Result=false);
	functional_syntax(B,B1),(B1==false,Result=false;B1==true,Result=true)
	).
functional_syntax_((A;B),Result) :-
	functional_syntax(A,A1),
	(A1==true,Result=true;A1==false,functional_syntax(B,Result)).

member1(El, [H|T]) :-
  member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
	member_(T, El, H).

unify_if_subsumes(A,B) :- subsumes_term(A,B),A=B.
