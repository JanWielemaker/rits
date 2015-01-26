:- module(rits_interaction, [
                             solve_with_student/1,
                             multiple_choice_sample/0
                            ]).

:- use_module(rits).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Tasks for students. (task number, expression)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

example(1, 1/2 + 1/2).
example(2, 1/4 + 2/4).
example(3, 1/3 + 1/2).
example(4, 1/5 + 2/3).

read_answer(T) :-
        read(R),
        (   var(R) ->
            format("    please enter a concrete solution\n"),
            read_answer(T)
        ;   integer(R) -> T = R
        ;   R = A / B -> T = A / B
        ;   atom(R) -> T = R
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
        (   Action1 == done ->
            rits_history(S, Hist),
            format("the interaction history: ~q\n", [Hist])
        ;   (   interpret_action(Action1, Action) -> true
            ;   throw(cannot_handle-Action1)
            ),
            interact_until_done(Action, S)
        ).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpret RITS actions.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- discontiguous interpret_action/2.

% interpret_action(A, _) :-
%         \+ functor(A, format, _),
%         format("\n\nINTERPRETING: ~w\n\n", [A]), false.
interpret_action(enter, next).
interpret_action(exit, next).
interpret_action(format(F,As), next)       :- format(F, As).
interpret_action(format(F), next)          :- format(F).
interpret_action(read_answer, student_answers(T)) :- nl, read_answer(T).
interpret_action(solve(Expr), solve(Expr)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Custom action for multimedia.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

interpret_action(video(V), next) :- format("Please watch: ~w\n", [V]).

rits:solve(lesson_1) -->
        [video("http://www.youtube.com/VideoAboutMozambique"),
         solve(mchoice("What is the capitol of Mozambique?\n",
                       ['Maputo','Pretoria','Nairobi','Vienna'], 1)),
         done].

%?- solve_with_student(lesson_1).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Custom actions for fraction domain.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

interpret_action(fraction_layout(F), next) :- fraction_layout(F).

fraction_layout(A + B) :- fraction_layout(A), format(" + "), fraction_layout(B).
fraction_layout(A/B)   :- format("~w/~w", [A,B]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Custom actions for multiple choice sample.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

interpret_action(translation(T), next) :- format("~w", [T]). % TODO: provide translations etc.
interpret_action(choices(Cs), next) :-
        foldl(format_choice, Cs, 1, _).

format_choice(C, N0, N) :-
        format("  ~w: ~w\n", [N0,C]),
        N is N0 + 1.

multiple_choice_sample :-
        solve_with_student(mchoice("What is the capitol of Mozambique?\n",
                                   ['Maputo', 'Pretoria', 'Nairobi', 'Vienna'],
                                   1)).


%?- multiple_choice_sample.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(setq ediprolog-prefix "")

?- solve_with_student(cancel(4/8)).

?- solve_with_student(cm(2,4)).

?- solve_with_student(1/2+3/4).

?- rits:rits_run_test([solve(1/2+3/4),*,=>(5/0),"wrong",*]).

?- rits:rits_run_test([solve(cancel(10/5)),*,=>(2/0),*]).

?- solve_with_student(1/2-3/4).

?- rits:rits_run_tests.

?- rits:rits_run_test([solve(1/2 + 3/4),*,=>(4/6),*,solve(_),*,=>(4),*,solve(_),*,=>(5/4),"nice"]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */