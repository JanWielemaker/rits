:- module(rits_multiple_choice, []).

rits:solve(mchoice(Text,Options,_)) --> [translation(Text),choices(Options)].


rits:actions(mchoice(_,_,Solution), Answer, _) -->
        { Solution == Answer },
        [format("Correct. Very nice!\n\n")].
rits:actions(mchoice(Text,Options,Solution), Answer, _) -->
        { \+ integer(Answer) },
        [format("The answer must be the integer corresponding to the solution.\n"),
         solve(mchoice(Text,Options,Solution))].
rits:actions(mchoice(Text,Options,Solution), _, _) -->
        [format("Wrong.\n"),
         solve(mchoice(Text,Options,Solution))].
