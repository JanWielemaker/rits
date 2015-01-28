
:- module(rits_common_multiple, [least_common_multiple/3]).

:- use_module(lorits).

rits:solve(cm(X,Y)) -->
        format("Please enter a common multiple of ~w and ~w:\n\n", [X,Y]).

rits:actions(cm(_,_), Answer, _) -->
        { \+ integer(Answer) },
        "A common multiple must be an integer!\n",
        again.
rits:actions(cm(X,Y), Answer, _) -->
        { Answer = 0, ( X =\= 0 ; Y =\= 0) },
        wrong,
        "The solution must be greater than 0.\n",
        again.
rits:actions(cm(X,Y), Answer, _) -->
        { Answer mod X =:= 0,
          Answer mod Y =:= 0 },
        "Good, the solution is correct",
        (   { least_common_multiple(X, Y, Answer) } ->
            " and also minimal. Very nice!\n\n"
        ;   ". There is also a smaller solution!\n"
        ).
rits:actions(cm(X,Y), Answer, Hist) -->
        wrong,
        help_for_wrong_answer(cm(X,Y), Answer, Hist),
        again.

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

help_for_wrong_answer(cm(X,Y), _, Hist) -->
        { Hist = [cm(X,Y)=_,cm(X,Y)=_,cm(X,Y)=_|_] },
        "I see you are having a hard time with this.\n",
        { CM is X*Y },
        format("Hint: ~w * ~w = ~w is a possible solution.\n", [X,Y,CM]).
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod X =\= 0 },
        format("~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X]).
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod Y =\= 0 },
        format("~w is no common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,Y]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Test cases.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rits:test([solve(cm(1,2)),*,=>(3),"not divisible",*]).
rits:test([solve(cm(1,2)),*,=>(3),"not divisible",*,solve(cm(1,2)),"again",*,=>(2),"nice"]).
rits:test([solve(cm(1,2)),*,=>(2),"minimal"]).
rits:test([solve(cm(1,2)),"multiple",=>(4),"smaller"]).
