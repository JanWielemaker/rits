:- module(rits_multiple_choice, []).

:- use_module(lorits).

rits:solve(mchoice(Text,Options,_)) --> [translation(Text),choices(Options)].


rits:actions(mchoice(_,_,Solution), Answer, _) -->
        { Solution == Answer },
        "Correct. Very nice!\n\n".
rits:actions(mchoice(_,_,_), Answer, _) -->
        { \+ integer(Answer) },
        "The answer must be the integer corresponding to the solution.\n",
        again.
rits:actions(mchoice(_,_,_), _, _) -->
        "Wrong.\n",
        again.
