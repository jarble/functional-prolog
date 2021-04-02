:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :- functional_syntax((
            print1(factorial(3)+factorial(4)),
            Concatenated_string = append("hello",append(" ","world")),
            print1(Concatenated_string),
            print1(length(Concatenated_string)),
            print1(nth0(0,Concatenated_string)),
            print1(msort([1,3,2,15,-1])),
            Z = factorial(2)**factorial(2),
            print1(Z > 2),
            ((Z==2) -> print1(Z); (Z>1) -> print1(Z)),
            {writeln(Z+1),nl,(nonvar(Z)->writeln(Z))},
            R = (3 =< 4),
            R1 = (3 >= 4),
            print1((R1,R2))
        ),true).

factorial(N,Output) :-
    functional_syntax((
        (N=1 -> Output = 1);
        Output = N*factorial(N-1)
    )).
print1(N,true) :-
	writeln(N).

functional_syntax(A) :- functional_syntax(A,true).
functional_syntax(A,A) :- number(A);var(A);atom(A).
functional_syntax(A>=B,C) :- functional_syntax(B =< A,C).
functional_syntax(A=<B,C) :- functional_syntax((A<B;A==B),C).
functional_syntax({A},true) :- A.
functional_syntax(not(X),Output) :-
    functional_syntax((X == false),Output).
functional_syntax(Input,C) :-
	Input =.. [Name|Params],
    member1(Name,[+,-,*,/,^,**]),
    functional_syntax(Params,Params1),
    Output =.. [Name|Params1],
    C is Output.

functional_syntax(A=B,Result) :-
    %variable assignment
    functional_syntax(B,B1),
    (A=B1,Result=true;A\=B1,Result=false).
    
functional_syntax(A==B,Result) :-
    %comparison of variables
    functional_syntax([A,B],[A1,B1]),
    (A1==B1,Result=true;A1\=B1,Result=false).

functional_syntax(A<B,Result) :-
	functional_syntax(B>A,Result).
functional_syntax(A>B,Result) :-
    %comparison of variables
    functional_syntax([A,B],[A1,B1]),
    (A1>B1,Result=true;A1=<B1,Result=false).
    
functional_syntax(A \= B,Result) :-
	functional_syntax(not(A==B),Result).
functional_syntax((A->B),Result) :-
    (functional_syntax(A,A1),A1=true) -> (functional_syntax(B,B1),Result=true,B1=true);
    Result=false.
functional_syntax([],[]).
functional_syntax([A|B],[A1|B1]) :-
    functional_syntax(A,A1),functional_syntax(B,B1).
functional_syntax((A,B),Result) :-
    functional_syntax([A,B],[A1,B1]),
    (A1,B1,Result=true;([A1,B1]=[true,false];[A1,B1]=[false,true]),Result=false).
functional_syntax((A;B),Result) :-
    (functional_syntax(A,A1),call(A1);
    functional_syntax(B,B1),call(B1)) -> (Result = true);
    (functional_syntax(A,A1),A1=false,Result=false).
functional_syntax(Input,Output1) :-
    (\+number(Input)),
    Input =.. [Name|Params],
    \+member1(Name,['=',==,'->',not,'[|]',',',';',+,-,*,/,**,^,<,>,=<,>=,writeln]),
    length(Params,Params_length),
    Params_length > 0,
    functional_syntax(Params,Params1),
    append([Name|Params1],[Output1],Input0),
    
    Input1 =.. Input0,
    call(Input1).

member1(El, [H|T]) :-
  member_(T, El, H).

member_(_, El, El).
member_([H|T], El, _) :-
	member_(T, El, H).
