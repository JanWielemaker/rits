/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   LORITS predefined actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(lorits, [
                   wrong//0,
                   again//0
                  ]).

wrong --> [format("This is wrong!\n")].

again --> [again].
