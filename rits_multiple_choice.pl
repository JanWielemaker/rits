:- module(rits_multiple_choice, []).

rits:solve(mchoice(Text,Options,_)) --> [translation(Text),choices(Options)].

rits:actions(mchoice(Text,Options,Solution), Answer, _) -->
        (   { Solution == Answer } ->
            [format("Correct. Very nice!\n\n")]
        ;   [format("Wrong.\n")],
            [solve(mchoice(Text,Options,Solution))]
        ).
