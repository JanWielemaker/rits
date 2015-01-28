
:- module(rits_fractions, []).

:- use_module(rits_common_multiple).
:- use_module(lorits).

to_rational(A, A)          :- integer(A), !.
to_rational(A0+B0, A + B)  :- !, to_rational(A0, A), to_rational(B0, B).
to_rational(A/B, A rdiv B) :- !.

rits:solve(A/B + C/D) -->
        "Please solve:\n\n~t~10+",
        [fraction_layout(A/B + C/D)].
rits:solve(cancel(X/Y)) -->
        "Please cancel common divisors in:\n\n~t~10+",
        [fraction_layout(X/Y)].


rits:actions(cancel(A/B), Answer0, Hist) -->
        (   { Answer0 = X / Y } ->
            (   { Y = 0 } ->
                "The denominator of a fraction cannot be 0.\n",
                 again
            ;   { A rdiv B =:= X rdiv Y } ->
                "Good, the solution is correct",
                (   { gcd(X,Y) =:= 1 } ->
                    " and also minimal. Very nice!\n\n"
                ;   ", but not minimal.\n",
                    solve(cancel(X/Y))
                )
            ;   help_for_wrong_answer(cancel(A/B), Answer0, Hist)
            )
        ;   { integer(Answer0) } ->
            (   { A mod B =:= 0, Answer0 =:= A//B } ->
                "Good, the solution is correct and also minimal. Very nice!\n\n"
            ;   help_for_wrong_answer(cancel(A/B), Answer0, Hist)
            )
        ;   "The answer must be an integer or a fraction.\n",
             again
        ).
rits:actions(Expression0, Answer0, Hist) -->
        { to_rational(Expression0, Expression) },
        (   { to_rational(Answer0, Answer) } ->
            (   { catch(Expression =:= Answer,_,false) } ->
                "Good, the solution is correct",
                { Shorter is Answer },
                (   { Shorter = Answer } ->
                    " and also minimal. Very nice!\n\n"
                ;   ", but not minimal.\n",
                    solve(cancel(Answer0))
                )
            ;   help_for_wrong_answer(Expression0, Answer0, Hist)
            )
        ;   wrong,
            "The answer must be an integer or a fraction.\n",
            again
        ).

help_for_wrong_answer(Expr, Answer, Hist) -->
        wrong,
        help_for_wrong_answer_(Expr, Answer, Hist),
        again.

help_for_wrong_answer_(cancel(A/B), _, Hist) -->
        { Hist = [cancel(A/B)=_,cancel(A/B)=_,cancel(A/B)=_|_] },
        "I see you are having a hard time with this.\n",
        format("Hint: Find a common divisor of ~w and ~w.\n", [A,B]).
help_for_wrong_answer_(_/B + _/D, _/Y, _) -->
        { least_common_multiple(B, D, Y) },
        "The denominator is suitable, but the numerator is wrong!\n".
help_for_wrong_answer_(_/B + _/D, _/Y, _) -->
        { Y mod B =:= 0,
          Y mod D =:= 0 },
        "The denominator is suitable, but the numerator is wrong!\n",
        "Use a smaller common multiple as denominator to make this easier.\n".
help_for_wrong_answer_(A/B + C/D, X / _, Hist) -->
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
help_for_wrong_answer_(A/B + C/D, Answer0, _) -->
        { to_rational(Answer0, Answer),
          Answer =:= (A + C) rdiv (B + D) },
        "You should not sum the denominators, but only the numerators!\n".
help_for_wrong_answer_(_/B + _/_, _ / Y, _) -->
        { Y mod B =\= 0 },
        format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,B]).
help_for_wrong_answer_(_/_ + _/D, _ / Y, _) -->
        { Y mod D =\= 0 },
        format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,D]).

% Fallback

help_for_wrong_answer_(_, _, _) -->
        "Unfortunately, I cannot give any useful hints here.\n".

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rits:test([solve(1/2 + 3/4),*,=>(5/4),"nice"]).
rits:test([solve(1/2 + 3/4),*,=>(4/6),*,solve(_),*,=>(4),*,solve(_),*,=>(5/4),"nice"]).
rits:test([solve(1/2+3/4),*,=>(a),*]).
