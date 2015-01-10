/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tasks for students. (task number, expression)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

example(1, 1/2 + 1/2).
example(2, 1/4 + 2/4).
example(3, 1/3 + 1/2).
example(4, 1/5 + 2/3).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The main logic for helping with wrong answers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

help_for_wrong_answer(cm(_,_), X) :-
        \+ integer(X),
        format("    A common multiple must be an integer!\n").
help_for_wrong_answer(cm(X,Y), A) :-
        A mod X =\= 0,
        format("    ~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X]).
help_for_wrong_answer(cm(X,Y), A) :-
        A mod Y =\= 0,
        format("    ~w is no common multiple of ~w and ~w, since ~w is not divisible ~w!\n", [A,X,Y,A,Y]).

help_for_wrong_answer(A/B + C/D, X / _) :-
        B =\= D,
        X =:= A + C,
        format("    You cannot just sum the numerators when the denominators are different!\n\n"),
        format("    Let us first find a common multiple of ~w and ~w!\n", [B,D]),
        solve_with_student(cm(B,D)).
help_for_wrong_answer(A/B + C/D, Answer0) :-
        to_rational(Answer0, Answer),
        Answer =:= (A + C) rdiv (B + D),
        format("    You should not sum the denominators, but only the numerators!\n").
help_for_wrong_answer(_, _) :-
        format("    Unfortunately, I cannot give any useful hints here.\n").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Program entry point, controlling the interactive session.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

run_examples(Fs) :- maplist(run_example, Fs).
run_example(N) :-
        example(N, Goal),
        excursion(solve_with_student(Goal), true).

excursion(Target, Goal) :- catch(Target, back, Goal).

fraction_layout(A + B) :- fraction_layout(A), write(" + "), fraction_layout(B).
fraction_layout(A/B)   :- format("~w/~w", [A,B]).

to_rational(A, A)          :- integer(A), !.
to_rational(A0+B0, A + B)  :- !, to_rational(A0, A), to_rational(B0, B).
to_rational(A/B, A rdiv B) :- !.

read_answer(T) :-
        read(R),
        (   var(R) ->
            format("    please enter a concrete solution\n"),
            read_answer(T)
        ;   integer(R) -> T = R
        ;   R = A / B -> T = A / B
        ;   format("    please enter a concrete solution\n"),
            read_answer(T)
        ).

solve_with_student(Expression) :- once(solve_with_student_(Expression)).

solve_with_student_(Expression) :-
        Expression = cm(X,Y),
        format("\nPlease enter a common multiple of ~w and ~w (solution + \".\" + RET):\n\n", [X,Y]),
        read_answer(Answer),
        nl,
        next(Expression, Answer, Next),
        do_next(Next, Expression).
solve_with_student_(Expression) :-
        format("\nPlease solve (solution + \".\" + RET):\n\n~t~10+"),
        fraction_layout(Expression),
        nl, nl,
        read_answer(Answer),
        nl,
        next(Expression, Answer, Next),
        do_next(Next, Expression).

do_next(done, _).
do_next(repeat, Expr) :-
        format("    So, let's try again!\n"),
        solve_with_student(Expr).
do_next(excursion(Exc), Expr) :- excursion(Exc), do_next(repeat, Expr).

excursion(help_for_wrong_answer(E, A)) :- once(help_for_wrong_answer(E, A)).

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

next(Expression, Answer, Next) :- once(next_(Expression, Answer, Next)).

next_(cm(X,Y), Answer, Next) :-
        (   Answer mod X =:= 0,
            Answer mod Y =:= 0 ->
            format("    Good, the solution is correct"),
            least_common_multiple(X, Y, LCM),
            (   Answer =:= LCM -> format(" and also minimal. Very nice!\n\n"),
                Next = done
            ;   format(". There is also a smaller solution!\n"),
                Next = done
            )
        ;   format("    This is wrong.\n"),
            Next = excursion(help_for_wrong_answer(cm(X,Y), Answer))
        ).
next_(Expression0, Answer0, Next) :-
        to_rational(Expression0, Expression),
        to_rational(Answer0, Answer),
        (   Expression =:= Answer ->
            format("    Good, the solution is correct"),
            Shorter is Answer,
            (   Shorter = Answer ->
                format(" and also minimal. Very nice!\n\n"),
                Next = done
            ;   format(", but not minimal.\n"),
                Next = repeat
            )
        ;   format("    This is wrong.\n"),
            Next = excursion(help_for_wrong_answer(Expression0, Answer0))
        ).

run :- solve_with_student(1/2 + 3/4).

/** <examples>

?- solve_with_student(1/2 + 3/4).

?- run_examples([1,2,4]).

*/
