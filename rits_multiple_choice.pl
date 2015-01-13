:- module(rits_multiple_choice, []).

rits:solve(mchoice(Text,Options,_)) -->
        [translation(Text),
         choices(Options),
         read_answer].

rits:actions(mchoice(Text,Options,Solution), Answer, _) -->
        (   { sort(Answer, Ls), sort(Solution, Ls) } ->
            [format("Correct. Very nice!\n\n")]
        ;   [format("Wrong.\n")],
            [solve(mchoice(Text,Options,Solution))]
        ).
