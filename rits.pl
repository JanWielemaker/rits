
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RRRR    II  TTTTTTTT  SSSS
   R  R    II     TT    SS             Rule-based Intelligent
   RRRR    II     TT     SSSSS         Tutoring System.
   R   R   II     TT         SS
   R   RR  II     TT   SSSSSSS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

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

% help_for_wrong_answer(_, _, Hist0, _) :-
%         format("HISTORY so far: ~w\n", [Hist0]),
%         false.

% Help for cancellation

help_for_wrong_answer(cancel(A/B), _, Hist, Hist) :-
        (   Hist = [cancel(Expr)-_,cancel(Expr)-_|_],
            format("    I see you are having a hard time with this.\n"),
            format("    Hint: Find a common divisor of ~w and ~w.\n", [A,B])
        ;   true
        ).

% Help for common multiple

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

% Help for fractions

help_for_wrong_answer(_/B + _/D, _/Y, Hist, Hist) :-
        least_common_multiple(B, D, Y),
        format("    The denominator is suitable, but the numerator is wrong!\n").
help_for_wrong_answer(_/B + _/D, _/Y, Hist, Hist) :-
        Y mod B =:= 0,
        Y mod D =:= 0,
        format("    The denominator is suitable, but the numerator is wrong!\n"),
        format("    Use a smaller common multiple as denominator to make this easier.\n").
help_for_wrong_answer(A/B + C/D, X / _, Hist0, Hist) :-
        B =\= D,
        X =:= A + C,
        format("    You cannot just sum the numerators when the denominators are different!\n\n"),
        (   member(cm(B,D)-Answer, Hist0), least_common_multiple(B,D,Answer) ->
            format("    Recall that you have already found the least common multiple of ~w and ~w!\n", [B,D]),
            format("    First rewrite the fractions so that the denominator is ~w for both, then add.\n", [Answer])
        ;   member(cm(B,D)-Answer, Hist0), Answer mod B =:= 0, Answer mod D =:= 0 ->
            format("    Recall that you have already found a common multiple of ~w and ~w: ~w\n", [B,D,Answer]),
            format("    You can either use that, or find a smaller multiple to make it easier.\n")
        ;   format("    Let us first find a common multiple of ~w and ~w!\n", [B,D]),
            solve_with_student(cm(B,D), Hist0, Hist)
        ).
help_for_wrong_answer(A/B + C/D, Answer0, Hist, Hist) :-
        to_rational(Answer0, Answer),
        Answer =:= (A + C) rdiv (B + D),
        format("    You should not sum the denominators, but only the numerators!\n").
help_for_wrong_answer(_/B + _/_, _ / Y, Hist, Hist) :-
        Y mod B =\= 0,
        format("    ~w cannot be a common denominator, because it cannot be diveded by ~w.\n", [Y,B]).
help_for_wrong_answer(_/_ + _/D, _ / Y, Hist, Hist) :-
        Y mod D =\= 0,
        format("    ~w cannot be a common denominator, because it cannot be diveded by ~w.\n", [Y,D]).

% Fallback

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
        do_next(Next, Expression, Answer, Hist0, Hist).
solve_with_student_(Expression, Hist0, Hist) :-
        Expression = cancel(X/Y),
        format("\nPlease cancel common divisors in:\n\n~t~10+"),
        fraction_layout(X/Y),
        nl, nl,
        read_answer(Answer),
        nl,
        next(Expression, Answer, Hist0, Next),
        do_next(Next, Expression, Answer, Hist0, Hist).
solve_with_student_(Expression, Hist0, Hist) :-
        format("\nPlease solve (solution + \".\" + RET):\n\n~t~10+"),
        fraction_layout(Expression),
        nl, nl,
        read_answer(Answer),
        nl,
        next(Expression, Answer, Hist0, Next),
        do_next(Next, Expression, Answer, Hist0, Hist).

do_next(done, Expr, Answer, Hist0, [Expr-Answer|Hist0]).
do_next(repeat, Expr, Answer, Hist0, Hist) :-
        format("    So, let's try again!\n"),
        solve_with_student(Expr, [Expr-Answer|Hist0], Hist).
do_next(excursion(Exc), Expr, Answer, Hist0, Hist) :-
        once(excursion(Exc, Hist0, Hist1)),
        do_next(repeat, Expr, Answer, Hist1, Hist).
do_next(continue(Cont), Expr, Answer, Hist0, Hist) :-
        solve_with_student(Cont, [Expr-Answer|Hist0], Hist).

% so that SWISH can see it is safe
excursion(help_for_wrong_answer(E, A), Hist0, Hist) :-
        help_for_wrong_answer(E, A, Hist0, Hist).

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

next(Expression, Answer, Hist, Next) :-
        once(next_(Expression, Answer, Hist, Next)).

next_(cm(X,Y), Answer, _, Next) :-
        (   \+ integer(Answer) -> Next = repeat,
            format("    A common multiple must be an integer!\n")
        ;   (   Answer mod X =:= 0,
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
            )
        ).
next_(cancel(A/B), Answer0, _, Next) :-
        (   Answer0 = X / Y ->
            (   A rdiv B =:= X rdiv Y ->
                format("    Good, the solution is correct"),
                (   gcd(X,Y) =:= 1 ->
                    format(" and also minimal. Very nice!\n\n"),
                    Next = done
                ;   format(", but not minimal.\n"),
                    Next = continue(cancel(X/Y))
                )
            ;   format("This is wrong!\n"),
                Next = excursion(help_for_wrong_answer(cancel(A/B), Answer0))
            )
        ;   integer(Answer0) ->
            (   A mod B =:= 0, Answer0 =:= A// B ->
                format("    Good, the solution is correct and also minimal. Very nice!\n\n"),
                Next = done
            ;   format("This is wrong!\n"),
                Next = excursion(help_for_wrong_answer(cancel(A/B), Answer0))
            )
        ;   Next = repeat
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
                Next = continue(cancel(Answer0))
            )
        ;   format("    This is wrong.\n"),
            Next = excursion(help_for_wrong_answer(Expression0, Answer0))
        ).

run :- solve_with_student(1/2 + 3/4).

/** <examples>

?- solve_with_student(1/2 + 3/4).

?- solve_with_student(cancel(10/8)).
*/
