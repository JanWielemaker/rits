%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
help_for_wrong_answer(A rdiv B + C rdiv D, X rdiv _) :-
        B =\= D,
        X =:= A + C,
        format("    You cannot just sum the numerators when the denominators are different!\n"),
        format("    Hint: Find a common multiple of ~w and ~w.\n", [B,D]).
help_for_wrong_answer(A rdiv B + C rdiv D, Answer) :-
        Answer =:= (A + C) rdiv (B + D),
        format("    You should not sum the denominators, but only the numerators!\n").
help_for_wrong_answer(_, _) :-
        format("    Unfortunately, I cannot give any useful hints here.\n").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
        ;   R = A / B -> T = A rdiv B
        ;   format("    please enter a concrete solution\n"),
            read_answer(T)
        ).
solve_with_student(Expression0) :-
        format("\nPlease solve (solution + \".\" + RET):\n\n~t~10+"),
        fraction_layout(Expression0),
        nl, nl,
        read_answer(Answer),
        nl,
        to_rational(Expression0, Expression),
        next(Expression, Answer, Next),
        do_next(Next, Expression0).
do_next(done, _).
do_next(repeat, Expr) :-
        format("    So, let's try again!\n"),
        solve_with_student(Expr).
do_next(excursion(Exc), Expr) :- excursion(Exc), do_next(repeat, Expr).
excursion(help_for_wrong_answer(E, A)) :- help_for_wrong_answer(E, A).
next(Expression, Answer, Next) :-
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
            Next = excursion(help_for_wrong_answer(Expression, Answer))
        ).

/** <examples>
?- solve_with_student(1/2 + 3/4).
?- run_examples([1,2,4]).
*/
