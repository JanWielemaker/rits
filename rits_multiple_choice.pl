:- module(rits_multiple_choice, []).

:- use_module(lorits).

rits:solve(mchoice(Text,Options,_)) --> [translation(Text),choices(Options)].


rits:actions(mchoice(_,_,Solution), Answer, _) -->
        { Solution == Answer },
        "Correct. Very nice!\n\n".
rits:actions(mchoice(Text,Options,Solution), Answer, _) -->
        { \+ integer(Answer) },
        "The answer must be the integer corresponding to the solution.\n",
        solve(mchoice(Text,Options,Solution)).
rits:actions(mchoice(Text,Options,Solution), _, _) -->
        "Wrong.\n",
        solve(mchoice(Text,Options,Solution)).
