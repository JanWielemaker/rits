
:- module(rits_fractions, []).

:- use_module(rits_common_multiple).
:- use_module(lorits).
:- use_module(library(dcg/basics)).

to_rational(A, A)          :- integer(A), !.
to_rational(A0+B0, A + B)  :- !, to_rational(A0, A), to_rational(B0, B).
to_rational(integer_and(I, F0), R) :- !, to_rational(F0, R0), R is I + R0.
to_rational(A/B, A rdiv B) :- !.

% TODO: improve sandboxing so that the module qualifier is not needed
rits:rits_term(I) --> dcg_basics:integer(I).
rits:rits_term(A/B) -->
        dcg_basics:(integer(A), blanks, "/", blanks, integer(B)).
rits:rits_term(integer_and(I,A/B)) -->
        dcg_basics:(integer(I),blank,blanks,
                    integer(A),blanks,"/",blanks,integer(B)).

rits:solve(A/B + C/D) -->
        "Please solve:\n\n~t~10+",
        [fraction_layout(A/B + C/D)].
rits:solve(cancel(X/Y)) -->
        "Please simplify the fraction:\n\n~t~10+",
        [fraction_layout(X/Y)].

fraction_answer(F) :- integer(F).
fraction_answer(F) :- nonvar(F), F = A/B, integer(A), integer(B).
fraction_answer(integer_and(_, _)).

rits:actions(cancel(A/B), Answer0, _) -->
        (   { Answer0 = X / Y } ->
            (   { Y = 0 } ->
                wrong,
                "The denominator of a fraction cannot be 0.\n",
                again
            ;   { A rdiv B =:= X rdiv Y } ->
                "Good, the solution is correct",
                (   { gcd(X,Y) =:= 1, Y =\= 1 } ->
                    " and also minimal. Very nice!\n\n"
                ;   ", but not minimal.\n",
                    help, solve(cancel(X/Y))
                )
            ;   wrong, help, again
            )
        ;   { integer(Answer0) } ->
            (   { A mod B =:= 0, Answer0 =:= A//B } ->
                "Good, the solution is correct and also minimal. Very nice!\n\n"
            ;   wrong, help, again
            )
        ;   "Please enter an integer or a fraction.\n",
            again
        ).
rits:actions(_, _/0, _) -->
        wrong, "The denominator of a fraction cannot be 0.", again.
rits:actions(Expression0, Answer0, _) -->
        { to_rational(Expression0, Expression) },
        (   { fraction_answer(Answer0), to_rational(Answer0, Answer) } ->
            (   { catch(Expression =:= Answer,_,false) } ->
                "Good, the solution is correct",
                (   { Answer is Answer } ->
                    " and also minimal. Very nice!\n\n"
                ;   ", but not minimal.\n",
                    (   { fraction_answer(Answer0) } ->
                        solve(cancel(Answer0))
                    ;   solve(Expression0)
                    )
                )
            ;   wrong, help, again
            )
        ;   "Please enter an integer or a fraction.\n",
            again
        ).

help --> help(rits_fractions:help).

help(cancel(A/B), _, Hist) -->
        { Hist = [cancel(A/B)=_,cancel(A/B)=_,cancel(A/B)=_|_] },
        "I see you are having a hard time with this.\n",
        (   { A mod B =:= 0 } ->
            "Hint: Write this fraction as a single integer.\n"
        ;   format("Hint: Find a common divisor of ~w and ~w.\n", [A,B])
        ).

help(_, F, _) -->
        { \+ fraction_answer(F) },
        "Please write the fraction as x/y, where x and y are integers.\n".
help(_/B + _/D, _/Y, Hist) -->
        { least_common_multiple(B, D, Y) },
        (   { Hist = [_=_/Y,_=_/Y,_=_/Y|_] } ->
            "Do not just guess! Multiply correctly, then add!\n"
        ;   "The denominator is suitable, but the numerator is wrong!\n"
        ).
help(_/B + _/D, _/Y, _) -->
        { Y mod B =:= 0,
          Y mod D =:= 0 },
        "The denominator is suitable, but the numerator is wrong!\n",
        "Use a smaller common multiple as denominator to make this easier.\n".
help(A/B + C/D, X / _, Hist) -->
        { B =\= D,
          X =:= A + C },
        "You cannot just sum the numerators when the denominators are different!\n\n",
        (   { member(cm(B,D)=Answer, Hist), least_common_multiple(B,D,Answer) } ->
            format("Recall that you have already found the least common multiple of ~w and ~w!\n", [B,D]),
            format("First rewrite the fractions so that the denominator is ~w for both, then add.\n", [Answer])
        ;   { member(cm(B,D)=Answer, Hist), Answer =\= 0, Answer mod B =:= 0, Answer mod D =:= 0 } ->
            format("Recall that you have already found a common multiple of ~w and ~w: ~w\n", [B,D,Answer]),
             "You can either use that, or find a smaller multiple to make it easier.\n"
        ;   subproblem([format("Let us first find a common multiple of ~w and ~w!\n", [B,D]),
                        solve(cm(B,D)),
                        "Now apply this knowledge to the original task!\n"])
        ).
help(A/B + C/D, Answer0, _) -->
        { Answer0 = _/_,
          to_rational(Answer0, Answer),
          Answer =:= (A + C) rdiv (B + D) },
        "You should not sum the denominators, but only the numerators!\n".
help(_/B + _/_, _ / Y, _) -->
        { Y mod B =\= 0 },
        format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,B]).
help(_/_ + _/D, _ / Y, _) -->
        { Y mod D =\= 0 },
        format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,D]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rits:test([solve(1/2 + 3/4),*,=>(5/4),"nice"]).
rits:test([solve(1/2 + 3/4),*,=>(4/6),*,=>(4),*,=>(5/4),"nice"]).
rits:test([solve(1/2+3/4),*,=>(a),*]).
rits:test([solve(1/2 + 3/4),*,=>(4/6),*,=>(4),*,=>(10/8),"not minimal",*,"simplify",*]).
rits:test([solve(cancel(2/2)),*,=>(2/2),*,=>(2/2),*,=>(2/2),"single integer",*,=>(2),"wrong",*,=>(1),"nice"]).
rits:test([solve(1/2+3/4),*,=>(1/4),*,=>(2/4),*,=>(3/4),"guess",*]).
rits:test([solve(3/4+2/1),*,=>(1),"wrong",solve(_),*]).
rits:test([solve(1/2+3/4),*,=>(4/4+1/4),"integer or a fraction",*]).
