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
interpret_action(student_answers(_), next).
interpret_action(format(F,As), next)       :- format(F, As).
interpret_action(format(F), next)          :- format(F).
interpret_action(read_answer, student_answers(T)) :-
        nl,
        read(T),
        (   T == end_of_file -> throw(eof)
        ;   true
        ).
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
?- rits:rits_run_test([solve(3/4+2/1),*,=>(1),"wrong",solve(_),*]).
?- rits:rits_run_test([solve(3/4+2/1),*,=>(1),"wrong",solve(_),*]).

?- rits:rits_run_test([solve(1/2+3/4),*,=>(5/0),"wrong",*]).

?- rits:rits_run_test([solve(cancel(10/5)),*,=>(2/0),*]).

?- rits:rits_run_test([solve(1/2 + 3/4),*,=>(4/6),*,solve(_),*,=>(4),*,solve(_),*,=>(10/8),"not minimal",solve(_),"simplify",*]).
?- solve_with_student(cm(1,2)).
?- solve_with_student(1/2-3/4).

?- rits:rits_run_tests.
?- rits:rits_run_test([solve(1/2+3/4),*,=>(4/4+1/4),"integer or a fraction",*]).

?- rits:rits_run_test([solve(cancel(2/2)),*,=>(2/2),*,=>(2/2),*,=>(2/2),"single integer",*,=>(2),"wrong",*,=>(1),"nice"]).

?- solve_with_student(1/2+1/2).

?- rits:rits_run_test([solve(cancel(2/2)),*,=>(2/2),*,solve(_),**]).

?- rits:rits_run_test([solve(1/2+3/4),*,=>(a),*]).

?- rits:rits_run_test([solve(cm(2,4)),*,=>(2),*,solve(_),*,=>(2),*,solve(_),*,=>(2),"hard time",*]).

?- rits:rits_run_test([solve(1/2 + 3/4),*,=>(4/6),*,solve(_),*,=>(4),*,solve(_),*,=>(5/4),"nice"]).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */