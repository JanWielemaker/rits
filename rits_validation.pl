:- module(rits_validation, []).

:- use_module(lorits).

rits:actions(Task, X, _) -->
        { \+ ground(X) },
        "The answer cannot contain variables.",
        solve(Task).
