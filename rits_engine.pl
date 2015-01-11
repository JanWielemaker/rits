
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   RRRR    II  TTTTTTTT  SSSS
   R  R    II     TT    SS             Rule-based Intelligent
   RRRR    II     TT     SSSSS         Tutoring System.
   R   R   II     TT         SS
   R   RR  II     TT   SSSSSSS
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(rits_engine, [
                        rits_start/1,       % -S0
                        rits_next_action/4, % +Action0, -Action, +S0, -S
                        rits_history/2      % +S, -History
                       ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

   Main interface to RITS.

   Given an action and the current state, decide what to do next.

   A single student action may require several actions to respond
   (printing a message, giving a hint, asking the student etc.).

   The history is represented as a list of previous interactions.
   Initially, this list is empty. Internal actions and history items
   are used to recall what the current question is. They have the form
   internal(I) and do not appear to the outside.

   The state is a term s(Nexts,Hist). Nexts are actions that still
   need to be executed. It is a mixture of a stack and a queue: For a
   single student interaction, several actions might be put into the
   queue, and these are executed FIFO-style. However, an erroneous
   answer may trigger an auxiliary interaction, and these interactions
   are collectively pushed and executed LIFO-style.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

rits_start(s([],[])).

rits_history(s(_,Hist0), Hist) :- reverse(Hist0, Hist).

rits_next_action(Action0, Action, S0, S) :-
        rits_next_action_(Action0, Action1, S0, S1),
        (   internal(Action1) ->
            rits_next_action(Action1, Action, S1, S)
        ;   Action = Action1,
            S = S1
        ).

rits_next_action_(Action0, Action, S0, s(Nexts,Hist)) :-
        (   var(Action0) -> throw(action_uninstantiated)
        ;   true
        ),
        S0 = s(Nexts0,Hist0),
        (   phrase(next_actions(Action0,Hist0,Hist), As0) -> true
        ;   portray_clause(phrase(next_actions(Action0,Hist0,Hist),_)),
            throw(no_action_found(Action0,Hist0,Hist))
        ),
        append(As0, Nexts0, Nexts1),
        %format("the next actions are: ~w\n", [Nexts1]),
        nexts_action_nexts(Nexts1, Action, Nexts).

nexts_action_nexts([], done, []).
nexts_action_nexts([Action|Nexts], Action, Nexts).

is_format(format(_,_)).
is_format(format(_)).
is_format(fraction_layout(_)).

do_next(repeat, Expr, Answer, Hist0, Hist) :-
        format("    So, let's try again!\n"),
        solve_with_student(Expr, [Expr-Answer|Hist0], Hist).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The main logic for helping with wrong answers.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

% help_for_wrong_answer(_, _, Hist0, _) :-
%         format("HISTORY so far: ~w\n", [Hist0]),
%         false.


internal(internal(_)).

list_internals(Ls, Is) :-
        include(internal, Ls, Is0),
        maplist(arg(1), Is0, Is).

% Help for cancellation

help_for_wrong_answer(cancel(A/B), _, Hist) -->
        { list_internals(Hist, [cancel(A/B),cancel(A/B)|_]) },
        [format("I see you are having a hard time with this.\n"),
         format("Hint: Find a common divisor of ~w and ~w.\n", [A,B])].

% Help for common multiple

help_for_wrong_answer(cm(X,Y), _, Hist) -->
        { list_internals(Hist, [cm(X,Y)=_,cm(X,Y)=_,cm(X,Y)=_|_]) },
        [format("I see you are having a hard time with this.\n")],
        { CM is X*Y },
        [format("Hint: ~w * ~w = ~w is a possible solution.\n", [X,Y,CM])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod X =\= 0 },
        [format("~w is not a common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,X])].
help_for_wrong_answer(cm(X,Y), A, _) -->
        { A mod Y =\= 0 },
        [format("~w is no common multiple of ~w and ~w, since ~w is not divisible by ~w!\n", [A,X,Y,A,Y])].

% Help for fractions

help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { least_common_multiple(B, D, Y) },
        [format("The denominator is suitable, but the numerator is wrong!\n")].
help_for_wrong_answer(_/B + _/D, _/Y, _) -->
        { Y mod B =:= 0,
          Y mod D =:= 0 },
        [format("The denominator is suitable, but the numerator is wrong!\n"),
         format("Use a smaller common multiple as denominator to make this easier.\n")].
help_for_wrong_answer(A/B + C/D, X / _, Hist0) -->
        { B =\= D,
          X =:= A + C,
          list_internals(Hist0, Hist) },
        [format("You cannot just sum the numerators when the denominators are different!\n\n")],
        (   { member(cm(B,D)=Answer, Hist), least_common_multiple(B,D,Answer) } ->
            [format("Recall that you have already found the least common multiple of ~w and ~w!\n", [B,D]),
             format("First rewrite the fractions so that the denominator is ~w for both, then add.\n", [Answer])]
        ;   { member(cm(B,D)=Answer, Hist), Answer mod B =:= 0, Answer mod D =:= 0 } ->
            [format("Recall that you have already found a common multiple of ~w and ~w: ~w\n", [B,D,Answer]),
             format("You can either use that, or find a smaller multiple to make it easier.\n")]
        ;   [format("Let us first find a common multiple of ~w and ~w!\n", [B,D]),
             solve(cm(B,D)),
             format("Now apply this knowledge to the original task!\n")]
        ).
help_for_wrong_answer(A/B + C/D, Answer0, _) -->
        { to_rational(Answer0, Answer),
          Answer =:= (A + C) rdiv (B + D) },
        [format("You should not sum the denominators, but only the numerators!\n")].
help_for_wrong_answer(_/B + _/_, _ / Y, _) -->
        { Y mod B =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,B])].
help_for_wrong_answer(_/_ + _/D, _ / Y, _) -->
        { Y mod D =\= 0 },
        [format("~w cannot be a common denominator, because it cannot be divided by ~w.\n", [Y,D])].

% Fallback

help_for_wrong_answer(_, _, _) -->
        [format("Unfortunately, I cannot give any useful hints here.\n")].


to_rational(A, A)          :- integer(A), !.
to_rational(A0+B0, A + B)  :- !, to_rational(A0, A), to_rational(B0, B).
to_rational(A/B, A rdiv B) :- !.

next_actions(next, Hist, Hist) --> [].
next_actions(done, Hist, Hist) --> [].
next_actions(internal(I), Hist, [internal(I)|Hist]) --> [].
next_actions(student_answers(A), Hist0, Hist) -->
        { Hist0 = [internal(Expr)|Rest],
          Hist = [internal(Expr=A)|Rest] },
        nexts(Expr, A, Hist),
        !. % commit to first solution
next_actions(solve(Expression), Hist, [solve(Expression)|Hist]) -->
        (   { Hist = [_,solve(Expression)|_] } ->
            [format("So, let's try again!\n")]
        ;   []
        ),
        solve(Expression),
        !, % commit to first solution
        [internal(Expression),read_answer].

solve(cm(X,Y)) -->
        [format("Please enter a common multiple of ~w and ~w:\n\n", [X,Y])].
solve(cancel(X/Y)) -->
        [format("Please cancel common divisors in:\n\n~t~10+"),
         fraction_layout(X/Y)].
solve(Expression) -->
        [format("Please solve:\n\n~t~10+"),
         fraction_layout(Expression)].

least_common_multiple(X, Y, CM) :- CM is X*Y // gcd(X, Y).

nexts(cm(X,Y), Answer, Hist) -->
        (   { \+ integer(Answer) } ->
            [format("A common multiple must be an integer!\n"), solve(cm(X,Y))]
        ;   (   { Answer mod X =:= 0,
                  Answer mod Y =:= 0 } ->
                [format("Good, the solution is correct")],
                { least_common_multiple(X, Y, LCM) },
                (   { Answer =:= LCM } ->
                    [format(" and also minimal. Very nice!\n\n")]
                ;   [format(". There is also a smaller solution!\n")]
                )
            ;   [format("This is wrong.\n")],
                help_for_wrong_answer(cm(X,Y), Answer, Hist),
                [solve(cm(X,Y))]
            )
        ).
nexts(cancel(A/B), Answer0, Hist) -->
        (   { Answer0 = X / Y } ->
            (   { A rdiv B =:= X rdiv Y } ->
                [format("Good, the solution is correct")],
                (   { gcd(X,Y) =:= 1 } ->
                    [format(" and also minimal. Very nice!\n\n")]
                ;   [format(", but not minimal.\n"), solve(cancel(X/Y))]
                )
            ;   [format("This is wrong!\n")],
                help_for_wrong_answer(cancel(A/B), Answer0, Hist),
                [solve(cancel(A/B))]
            )
        ;   { integer(Answer0) } ->
            (   { A mod B =:= 0, Answer0 =:= A//B } ->
                [format("Good, the solution is correct and also minimal. Very nice!\n\n")]
            ;   [format("This is wrong!\n")],
                help_for_wrong_answer(cancel(A/B), Answer0, Hist),
                [solve(cancel(A/B))]
            )
        ;   [solve(cancel(A/B))]
        ).
nexts(Expression0, Answer0, Hist) -->
        { to_rational(Expression0, Expression),
          to_rational(Answer0, Answer) },
        (   { Expression =:= Answer } ->
            [format("Good, the solution is correct")],
            { Shorter is Answer },
            (   { Shorter = Answer } ->
                [format(" and also minimal. Very nice!\n\n")]
            ;   [format(", but not minimal.\n"), solve(cancel(Answer0))]
            )
        ;   [format("This is wrong.\n")],
            help_for_wrong_answer(Expression0, Answer0, Hist),
            [solve(Expression0)]
        ).

/** <examples>

?- rits_start(S0), rits_next_action(solve(1/2+3/4), A, S0, S).

*/
