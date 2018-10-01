:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :- functional_syntax((
            writeln(factorial(3)+factorial(4)),
            Concatenated_string = append("hello",append(" ","world")),
            writeln(Concatenated_string),
            writeln(length(Concatenated_string)),
            writeln(nth0(0,Concatenated_string)),
            writeln(msort([1,3,2,15,-1])),
            Z = factorial(2)**factorial(2),
            writeln(Z),
            {writeln(Z+1), (nonvar(Z)->writeln(Z))}
        ),true).

factorial(N,Output) :-
    functional_syntax((
        (N=1 -> Output = 1);
        Output = N*factorial(N-1)
    )).


functional_syntax(A) :- functional_syntax(A,true).
functional_syntax(A,A) :- number(A);var(A);atom(A).
functional_syntax({A},true) :- A.
functional_syntax(not(X),Output) :-
    functional_syntax((X = false),Output).
functional_syntax(writeln(A),true) :-
    functional_syntax(A,A1),writeln(A1).
functional_syntax(Input,C) :-
	Input =.. [Name|Params],
    member(Name,[+,-,*,/,^,**]),
    functional_syntax(Params,Params1),
    Output =.. [Name|Params1],
    C is Output.

functional_syntax(A=B,Result) :-
    functional_syntax(B,B1),
    (A=B1,Result=true;dif(A,B1),Result=false).
functional_syntax(A->B,Result) :-
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
    not(number(Input)),
    Input =.. [Name|Params],
    \+member(Name,['=','->',not,'[|]',',',';',+,-,*,/,**,^]),
    length(Params,Params_length),
    Params_length > 0,
    functional_syntax(Params,Params1),
    append([Name|Params1],[Output1],Input0),
    Input1 =.. Input0,
    call(Input1).
