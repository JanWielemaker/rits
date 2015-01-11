:- module(rits_interaction, [
                             solve_with_student/1
                            ].

:- use_module(rits).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tasks for students. (task number, expression)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

example(1, 1/2 + 1/2).
example(2, 1/4 + 2/4).
example(3, 1/3 + 1/2).
example(4, 1/5 + 2/3).


fraction_layout(A + B) :- fraction_layout(A), " + ", fraction_layout(B).
fraction_layout(A/B)   :- format("~w/~w", [A,B]).

read_answer(T) :-
        read(R),
        (   var(R) ->
            format("    please enter a concrete solution\n"),
            read_answer(T)
        ;   integer(R) -> T = R
        ;   R = A / B -> T = A / B
        ;   format("    please enter a concrete solution\n"),
            read_answer(T)
        ).


solve_with_student(Expression) :-
        rits_start(S0),
        rits_next_action(Expression, Action, S0, S1)