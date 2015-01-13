
:- module(rits_common_multiple, [least_common_multiple/3]).

rits:solve(cm(X,Y)) -->
        [format("Please enter a common multiple of ~w and ~w:\n\n", [X,Y])].

rits:actions(cm(X,Y), Answer, Hist) -->
        (   { \+ integer(Answer) } ->
            [format("A common multiple must be an integer!\n"), solve(cm(X,Y))]
        ;   (   { Answer =\= 0,
                  Answer mod X =:= 0,
                  Answer mod Y =:= 0 } ->
                [format("Good, the solution is correct")],
                { least_common_multiple(X, Y, LCM) },
                (   { Answer =:= LCM } ->
                    [format(" and also minimal. Very nice!\n\n")]
                ;   [format(". There is also a smaller solution!\n")]
                )
            ;   [format("This is wrong.\n")],
                help_for_wrong_answer(cm(X,Y), Answer, Hist),
                [solve(cm(X,Y))]
            )
        ).

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

help_for_wrong_answer(cm(X,Y), _, Hist) -->
        { Hist = [cm(X,Y)=_,cm(X,Y)=_,cm(X,Y)=_|_] },
        [format("I see you are having a hard time with this.\n")],
        { CM is X*Y },
        [format("Hint: ~w * ~w = ~w is a possible solution.\n", [X,Y,CM])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod X =\= 0 },
        [format("~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod Y =\= 0 },
        [format("~w is no common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,Y])].

