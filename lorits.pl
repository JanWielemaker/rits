/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LORITS predefined actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(lorits, [
                   again//0,
                   format//2,
                   solve//1,
                   subproblem//1,
                   wrong//0
                  ]).

wrong --> [format("This is wrong!\n")].

again --> [again].

format(Str, Args) --> [format(Str, Args)].

solve(Task) --> [solve(Task)].

subproblem(P) --> [subproblem(P)].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The fact "rits_help(Module:Pred)." makes the nonterminal Pred//0 in
   all DCG rules within Module a synonym for help(Module).

   In addition, DCG rules with head Pred(Task, Action, Hist) within
   Module are rewritten to rits:help(Module, Task, Action, Hist).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

user:term_expansion(rits_help(Module:Pred),
                    [Module:goal_expansion(HelpGoal, ModuleGoal),
                     Module:term_expansion(HelpDCG, MainDCG)]) :-
        HelpGoal =.. [Pred,Ls0,Ls],
        ModuleGoal = ( Ls0 = [help(Module)|Ls] ),
        DCGHead =.. [Pred,A,B,C],
        HelpDCG = (DCGHead --> Body),
        MainDCG = (rits:help(Module, A, B, C) --> Body).
