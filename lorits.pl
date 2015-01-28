/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LORITS predefined actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(lorits, [
                   again//0,
                   format//2,
                   help//1,
                   solve//1,
                   subproblem//1,
                   wrong//0,
                   wrong//1
                  ]).

wrong --> [format("This is wrong!\n")].

wrong(Str) --> wrong, [format(Str)], again.

again --> [again].

format(Str, Args) --> [format(Str, Args)].

solve(Task) --> [solve(Task)].

subproblem(P) --> [subproblem(P)].

help(Pred) --> wrong, [help(Pred)], again.
