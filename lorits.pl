/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LORITS predefined actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(lorits, [
                   again//0,
                   format//2,
                   help//1,
                   solve//1,
                   subproblem//1,
                   wrong//0
                  ]).

wrong --> [format("This is wrong!\n")].

again --> [again].

format(Str, Args) --> [format(Str, Args)].

solve(Task) --> [solve(Task)].

subproblem(P) --> [subproblem(P)].

help(Pred) --> wrong, [help(Pred)], again.
