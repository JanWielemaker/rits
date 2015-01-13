% Help for common multiple

help_for_wrong_answer(cm(X,Y), _, Hist) -->
        { list_internals(Hist, [cm(X,Y)=_,cm(X,Y)=_,cm(X,Y)=_|_]) },
        [format("I see you are having a hard time with this.\n")],
        { CM is X*Y },
        [format("Hint: ~w * ~w = ~w is a possible solution.\n", [X,Y,CM])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod X =\= 0 },
        [format("~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod Y =\= 0 },
        [format("~w is no common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,Y])].
