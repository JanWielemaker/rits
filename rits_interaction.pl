:- module(rits_interaction, [
                             solve_with_student/1
                            ]).

:- use_module(rits_engine).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tasks for students. (task number, expression)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

example(1, 1/2 + 1/2).
example(2, 1/4 + 2/4).
example(3, 1/3 + 1/2).
example(4, 1/5 + 2/3).


fraction_layout(A + B) :- fraction_layout(A), format(" + "), fraction_layout(B).
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
        interact_until_done(solve(Expression), S0).

interact_until_done(Action0, S0) :-
        rits_next_action(Action0, Action1, S0, S),
        %nl,
        %portray_clause(rits_next_action(Action0, Action1, S0, S)),
        (   Action1 == done -> true
        ;   once(interpret_action(Action1, Action)),
            interact_until_done(Action, S)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpret RITS actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% interpret_action(A, _) :-
%         \+ functor(A, format, _),
%         format("\n\nINTERPRETING: ~w\n\n", [A]), false.
interpret_action(format(F,As), next)       :- format(F, As).
interpret_action(format(F), next)          :- format(F).
interpret_action(fraction_layout(F), next) :- fraction_layout(F).
interpret_action(read_answer, student_answers(T)) :- nl, read_answer(T).
interpret_action(A, A).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(setq ediprolog-prefix "")

?- solve_with_student(cm(2,4)).

?- solve_with_student(1/2+3/4).

%?- gtrace, rits_engine:next_actions(student_answers(4/6), [next, next, solve(1/2+3/4)], _, Ys).

   
?- gtrace, rits_engine:next_actions(student_answers(3/4), [internal(1/2+3/4),read_answer], As, Rs). %

?- gtrace, solve_with_student(1/2+3/4).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */