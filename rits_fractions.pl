% Help for fractions

help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { least_common_multiple(B, D, Y) },
        [format("The denominator is suitable, but the numerator is wrong!\n")].
help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { Y mod B =:= 0,
          Y mod D =:= 0 },
        [format("The denominator is suitable, but the numerator is wrong!\n"),
         format("Use a smaller common multiple as denominator to make this easier.\n")].
help_for_wrong_answer(A/B + C/D, X / _, Hist0) -->
        { B =\= D,
          X =:= A + C,
          list_internals(Hist0, Hist) },
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
help_for_wrong_answer(A/B + C/D, Answer0, _) -->
        { to_rational(Answer0, Answer),
          Answer =:= (A + C) rdiv (B + D) },
        [format("You should not sum the denominators, but only the numerators!\n")].
help_for_wrong_answer(_/B + _/_, _ / Y, _) -->
        { Y mod B =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,B])].
help_for_wrong_answer(_/_ + _/D, _ / Y, _) -->
        { Y mod D =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,D])].
