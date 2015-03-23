:- module(rits_validation, []).

:- use_module(lorits).

rits:actions(Task, X, _) -->
        { var(X) },
        "The answer cannot be a variable.",
        solve(Task).
