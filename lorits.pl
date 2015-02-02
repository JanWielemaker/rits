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

help(Pred) --> [help(Pred)].


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   TODO: Find a way to structure the modules so that the following
         declarations are not necessary. These are provably safe.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(lorits:format(_,_,_,_)).
sandbox:safe_primitive(lorits:again(_,_)).
