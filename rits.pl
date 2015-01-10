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

help_for_wrong_answer(cm(_,_), X, H, H) :-
        \+ integer(X),
        format("    A common multiple must be an integer!\n").
help_for_wrong_answer(cm(X,Y), _, Hist, Hist) :-
        Hist = [cm(X,Y)-_,cm(X,Y)-_|_],
        format("    I see you are having a hard time with this.\n"),
        least_common_multiple(X, Y, CM),
        format("    Hint: ~w is a possible solution.\n", [CM]).
help_for_wrong_answer(cm(X,Y), A, Hist, Hist) :-
        A mod X =\= 0,
        format("    ~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X]).
help_for_wrong_answer(cm(X,Y), A, Hist, Hist) :-
        A mod Y =\= 0,
        format("    ~w is no common multiple of ~w and ~w, since ~w is not divisible ~w!\n", [A,X,Y,A,Y]).

help_for_wrong_answer(A/B + C/D, X / _, Hist0, Hist) :-
        B =\= D,
        X =:= A + C,
        format("    You cannot just sum the numerators when the denominators are different!\n\n"),
        (   member(cm(B,D)-Answer, Hist), least_common_multiple(B,D,Answer) ->
            format("    Recall that you have already found the least common multiple of ~w and ~w!\n", [B,D]),
            format("    First rewrite the fractions so that the denominator is ~w for both, then add.", [Answer])
        ;   member(cm(B,D)-Answer, Hist), Answer mod B =:= 0, Answer mod D =:= 0 ->
            format("    Recall that you have already found a common multiple of ~w and ~w: ~w\n", [B,D,Answer]),
            format("    You can either use that, or find a smaller multiple to make it easier.\n")
        ;   format("    Let us first find a common multiple of ~w and ~w!\n", [B,D]),
            solve_with_student(cm(B,D), Hist0, Hist)
        ).
help_for_wrong_answer(A/B + C/D, Answer0, Hist, Hist) :-
        to_rational(Answer0, Answer),
        Answer =:= (A + C) rdiv (B + D),
        format("    You should not sum the denominators, but only the numerators!\n").
help_for_wrong_answer(_, _, Hist, Hist) :-
        format("    Unfortunately, I cannot give any useful hints here.\n").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Program entry point, controlling the interactive session.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

solve_with_student(Expression) :- solve_with_student(Expression, [], _).

solve_with_student(Expression, Hist0, Hist) :-
        once(solve_with_student_(Expression, Hist0, Hist)).

solve_with_student_(Expression, Hist0, Hist) :-
        Expression = cm(X,Y),
        format("\nPlease enter a common multiple of ~w and ~w (solution + \".\" + RET):\n\n", [X,Y]),
        read_answer(Answer),
        nl,
        next(Expression, Answer, Hist0, Next),
        do_next(Next, Expression, [Expression-Answer|Hist0], Hist).
solve_with_student_(Expression, Hist0, Hist) :-
        format("\nPlease solve (solution + \".\" + RET):\n\n~t~10+"),
        fraction_layout(Expression),
        nl, nl,
        read_answer(Answer),
        nl,
        next(Expression, Answer, Hist0, Next),
        do_next(Next, Expression, [Expression-Answer|Hist0], Hist).

do_next(done, _, Hist, Hist).
do_next(repeat, Expr, Hist0, Hist) :-
        format("    So, let's try again!\n"),
        solve_with_student(Expr, Hist0, Hist).
do_next(excursion(Exc), Expr, Hist0, Hist) :-
        once(excursion(Exc), Hist0, Hist1),
        do_next(repeat, Expr, Hist1, Hist).

% so that SWISH can see it is safe
excursion(help_for_wrong_answer(E, A), Hist0, Hist) :-
        help_for_wrong_answer(E, A, Hist0, Hist).

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

next(Expression, Answer, Hist, Next) :-
        once(next_(Expression, Answer, Hist, Next)).

next_(cm(X,Y), Answer, _, Next) :-
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
next_(Expression0, Answer0, _, Next) :-
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

*/
