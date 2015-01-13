
:- use_module(rits_common_multiple).

rits:solve(A/B + C/D) -->
        [format("Please solve:\n\n~t~10+"),
         fraction_layout(Expression)].
rits:solve(cancel(X/Y)) -->
        [format("Please cancel common divisors in:\n\n~t~10+"),
         fraction_layout(X/Y)].


rits:help_for_wrong_answer(cancel(A/B), _, Hist) -->
        { list_internals(Hist, [cancel(A/B)=_,cancel(A/B)=_,cancel(A/B)=_|_]) },
        [format("I see you are having a hard time with this.\n"),
         format("Hint: Find a common divisor of ~w and ~w.\n", [A,B])].
rits:help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { least_common_multiple(B, D, Y) },
        [format("The denominator is suitable, but the numerator is wrong!\n")].
rits:help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { Y mod B =:= 0,
          Y mod D =:= 0 },
        [format("The denominator is suitable, but the numerator is wrong!\n"),
         format("Use a smaller common multiple as denominator to make this easier.\n")].
rits:help_for_wrong_answer(A/B + C/D, X / _, Hist) -->
        { B =\= D,
          X =:= A + C },
        [format("You cannot just sum the numerators when the denominators are different!\n\n")],
        (   { member(cm(B,D)=Answer, Hist), least_common_multiple(B,D,Answer) } ->
            [format("Recall that you have already found the least common multiple of ~w and ~w!\n", [B,D]),
             format("First rewrite the fractions so that the denominator is ~w for both, then add.\n", [Answer])]
        ;   { member(cm(B,D)=Answer, Hist), Answer mod B =:= 0, Answer mod D =:= 0 } ->
            [format("Recall that you have already found a common multiple of ~w and ~w: ~w\n", [B,D,Answer]),
             format("You can either use that, or find a smaller multiple to make it easier.\n")]
        ;   subproblem([format("Let us first find a common multiple of ~w and ~w!\n", [B,D]),
                        solve(cm(B,D)),
                        format("Now apply this knowledge to the original task!\n")])
        ).
rits:help_for_wrong_answer(A/B + C/D, Answer0, _) -->
        { to_rational(Answer0, Answer),
          Answer =:= (A + C) rdiv (B + D) },
        [format("You should not sum the denominators, but only the numerators!\n")].
rits:help_for_wrong_answer(_/B + _/_, _ / Y, _) -->
        { Y mod B =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,B])].
rits:help_for_wrong_answer(_/_ + _/D, _ / Y, _) -->
        { Y mod D =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,D])].

rits:nexts(cancel(A/B), Answer0, Hist) -->
        (   { Answer0 = X / Y } ->
            (   { A rdiv B =:= X rdiv Y } ->
                [format("Good, the solution is correct")],
                (   { gcd(X,Y) =:= 1 } ->
                    [format(" and also minimal. Very nice!\n\n")]
                ;   [format(", but not minimal.\n"), solve(cancel(X/Y))]
                )
            ;   [format("This is wrong!\n")],
                help_for_wrong_answer(cancel(A/B), Answer0, Hist),
                [solve(cancel(A/B))]
            )
        ;   { integer(Answer0) } ->
            (   { A mod B =:= 0, Answer0 =:= A//B } ->
                [format("Good, the solution is correct and also minimal. Very nice!\n\n")]
            ;   [format("This is wrong!\n")],
                help_for_wrong_answer(cancel(A/B), Answer0, Hist),
                [solve(cancel(A/B))]
            )
        ;   [solve(cancel(A/B))]
        ).
rits:nexts(Expression0, Answer0, Hist) -->
        { Expression0 = (A/B + C/D) },
        { to_rational(Expression0, Expression),
          to_rational(Answer0, Answer) },
        (   { Expression =:= Answer } ->
            [format("Good, the solution is correct")],
            { Shorter is Answer },
            (   { Shorter = Answer } ->
                [format(" and also minimal. Very nice!\n\n")]
            ;   [format(", but not minimal.\n"), solve(cancel(Answer0))]
            )
        ;   [format("This is wrong.\n")],
            help_for_wrong_answer(Expression0, Answer0, Hist),
            [solve(Expression0)]
        ).
