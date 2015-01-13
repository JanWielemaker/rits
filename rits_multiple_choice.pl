:- module(rits_multiple_choice, []).

rits:solve(mchoice(Text,Options,_)) -->
        [translation(Text),
         choices(Options)].

rits:actions(mchoice(Text,Options,Solution)) -->
        (   { sort(Options, Ls), sort(Solution, Ls) } ->
            [format("Correct. Very nice!\n\n")]
        ;   [format("Wrong.\n")],
            [solve(mchoice(Text,Options,Solution))]
        ).
